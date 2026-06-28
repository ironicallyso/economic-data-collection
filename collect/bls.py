from __future__ import annotations

import os
from datetime import date as _date
from typing import Dict, List, Optional, Tuple

import requests

from collect import io
from collect.config import AppConfig


def chunk_year_ranges(
    start_year: int, end_year: int, max_years_per_request: int
) -> List[Tuple[int, int]]:
    """Split [start_year, end_year] into inclusive windows of at most
    max_years_per_request years each, covering the full range with no gaps."""
    if max_years_per_request < 1:
        raise ValueError("max_years_per_request must be >= 1")
    chunks = []
    cur = start_year
    while cur <= end_year:
        chunk_end = min(cur + max_years_per_request - 1, end_year)
        chunks.append((cur, chunk_end))
        cur = chunk_end + 1
    return chunks


def fetch_series_chunk(
    series_ids: List[str],
    start_year: int,
    end_year: int,
    base_url: str,
    api_key: Optional[str] = None,
) -> Dict:
    payload = {
        "seriesid": series_ids,
        "startyear": str(start_year),
        "endyear": str(end_year),
    }
    if api_key:
        payload["registrationkey"] = api_key
    response = requests.post(base_url, json=payload, timeout=30)
    response.raise_for_status()
    return response.json()


def parse_bls_response(raw: Dict, units_by_series: Dict[str, str]) -> List[Dict]:
    rows = []
    for series in raw.get("Results", {}).get("series", []):
        series_id = series["seriesID"]
        units = units_by_series.get(series_id, "")
        for item in series.get("data", []):
            period = item["period"]
            if period == "M13":
                continue  # annual average, not a monthly observation
            raw_value = item["value"]
            if raw_value in ("-", ""):
                continue  # BLS placeholder for not-yet-published data
            month = int(period[1:])
            year = int(item["year"])
            rows.append(
                {
                    "series_id": series_id,
                    "date": f"{year:04d}-{month:02d}-01",
                    "value": float(raw_value),
                    "units": units,
                    "source": "BLS",
                }
            )
    return rows


def fetch_bls_series(
    series_ids: List[str],
    start_year: int,
    end_year: int,
    base_url: str,
    units_by_series: Dict[str, str],
    max_years_per_request: int,
    api_key: Optional[str] = None,
) -> List[Dict]:
    rows = []
    for chunk_start, chunk_end in chunk_year_ranges(
        start_year, end_year, max_years_per_request
    ):
        raw = fetch_series_chunk(series_ids, chunk_start, chunk_end, base_url, api_key)
        rows.extend(parse_bls_response(raw, units_by_series))
    return rows


def _lookback_start_year(today: _date, lookback_months: int) -> int:
    total_month_index = today.year * 12 + (today.month - 1) - lookback_months
    return total_month_index // 12


def run_historical(config: AppConfig) -> None:
    api_key = os.environ.get("BLS_API_KEY")
    bls = config.bls
    units_by_series = {s.id: s.units for s in bls.series}
    end_year = _date.today().year
    rows = fetch_bls_series(
        series_ids=[s.id for s in bls.series],
        start_year=bls.start_year,
        end_year=end_year,
        base_url=bls.base_url,
        units_by_series=units_by_series,
        max_years_per_request=bls.max_years_per_request,
        api_key=api_key,
    )
    io.upsert_csv(rows, bls.output_path)


def run_latest(config: AppConfig) -> None:
    api_key = os.environ.get("BLS_API_KEY")
    bls = config.bls
    units_by_series = {s.id: s.units for s in bls.series}
    today = _date.today()
    end_year = today.year
    start_year = _lookback_start_year(today, config.latest_lookback_months)
    rows = fetch_bls_series(
        series_ids=[s.id for s in bls.series],
        start_year=start_year,
        end_year=end_year,
        base_url=bls.base_url,
        units_by_series=units_by_series,
        max_years_per_request=bls.max_years_per_request,
        api_key=api_key,
    )

    cutoff_month_index = (
        today.year * 12 + (today.month - 1) - config.latest_lookback_months
    )
    cutoff_year = cutoff_month_index // 12
    cutoff_month = cutoff_month_index % 12 + 1
    cutoff_date = f"{cutoff_year:04d}-{cutoff_month:02d}-01"
    rows = [r for r in rows if r["date"] >= cutoff_date]

    io.upsert_csv(rows, bls.output_path)
