from __future__ import annotations

import os
from typing import Dict, List

import requests

from collect import io
from collect.config import AppConfig


def build_bea_params(
    api_key: str, dataset: str, table_name: str, frequency: str
) -> Dict:
    return {
        "UserID": api_key,
        "method": "GetData",
        "DataSetName": dataset,
        "TableName": table_name,
        "Frequency": frequency,
        "Year": "ALL",
        "ResultFormat": "JSON",
    }


def fetch_bea_table(base_url: str, params: Dict) -> Dict:
    response = requests.get(base_url, params=params, timeout=30)
    response.raise_for_status()
    return response.json()


def parse_bea_response(raw: Dict, line_number: int, units: str) -> List[Dict]:
    rows = []
    data = raw.get("BEAAPI", {}).get("Results", {}).get("Data", [])
    for row in data:
        if int(row["LineNumber"]) != line_number:
            continue
        time_period = row["TimePeriod"]
        year = int(time_period[:4])
        month = int(time_period[5:7])
        rows.append(
            {
                "series_id": row["SeriesCode"],
                "date": f"{year:04d}-{month:02d}-01",
                "value": float(row["DataValue"].replace(",", "")),
                "units": units,
                "source": "BEA",
            }
        )
    return rows


def _require_api_key() -> str:
    api_key = os.environ.get("BEA_API_KEY")
    if not api_key:
        raise SystemExit(
            "BEA_API_KEY not set in environment. BEA live collection is not yet "
            "available in Phase 1 -- parsing logic is implemented and tested against "
            "fixtures (see tests/test_bea.py), but no live calls are made without a key."
        )
    return api_key


def run_historical(config: AppConfig) -> None:
    api_key = _require_api_key()
    bea = config.bea
    rows = []
    for table in bea.tables:
        params = build_bea_params(api_key, bea.dataset, table.table_name, bea.frequency)
        raw = fetch_bea_table(bea.base_url, params)
        rows.extend(parse_bea_response(raw, table.line_number, table.units))
    io.upsert_csv(rows, bea.output_path)


def run_latest(config: AppConfig) -> None:
    # BEA's GetData call always returns the full table (Year=ALL); there is no
    # incremental request mode, so "latest" reuses the same full fetch+upsert path.
    run_historical(config)
