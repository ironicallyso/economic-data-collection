from __future__ import annotations

import os
from datetime import date as _date, timedelta
from typing import Dict, List

import requests

from collect import io
from collect.config import AppConfig


def fetch_fred_series(
    series_id: str,
    start_date: str,
    end_date: str,
    base_url: str,
    api_key: str,
) -> Dict:
    params = {
        "series_id": series_id,
        "api_key": api_key,
        "file_type": "json",
        "observation_start": start_date,
        "observation_end": end_date,
    }
    response = requests.get(base_url, params=params, timeout=30)
    response.raise_for_status()
    return response.json()


def parse_fred_response(raw: Dict, series_id: str, units: str) -> List[Dict]:
    rows = []
    for obs in raw.get("observations", []):
        raw_value = obs["value"]
        if raw_value == ".":
            continue  # FRED placeholder for missing/not-yet-published data
        rows.append(
            {
                "series_id": series_id,
                "date": obs["date"],
                "value": float(raw_value),
                "units": units,
                "source": "FRED",
            }
        )
    return rows


def _require_api_key() -> str:
    api_key = os.environ.get("FRED_API_KEY")
    if not api_key:
        raise SystemExit(
            "FRED_API_KEY not set in environment. FRED live collection requires "
            "a key (see .env.example)."
        )
    return api_key


def run_historical(config: AppConfig) -> None:
    api_key = _require_api_key()
    fred = config.fred
    end_date = _date.today().isoformat()
    rows = []
    for series in fred.series:
        raw = fetch_fred_series(series.id, fred.start_date, end_date, fred.base_url, api_key)
        rows.extend(parse_fred_response(raw, series.id, series.units))
    io.upsert_csv(rows, fred.output_path)


def run_latest(config: AppConfig) -> None:
    api_key = _require_api_key()
    fred = config.fred
    end_date = _date.today().isoformat()
    last_dates = io.latest_date_by_series(fred.output_path)

    rows = []
    for series in fred.series:
        last_date = last_dates.get(series.id)
        if last_date:
            start_date = (_date.fromisoformat(last_date) + timedelta(days=1)).isoformat()
        else:
            start_date = fred.start_date
        if start_date > end_date:
            continue  # already up to date
        raw = fetch_fred_series(series.id, start_date, end_date, fred.base_url, api_key)
        rows.extend(parse_fred_response(raw, series.id, series.units))

    if rows:
        io.upsert_csv(rows, fred.output_path)
