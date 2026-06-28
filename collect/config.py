from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional, Union

import yaml


@dataclass(frozen=True)
class BLSSeriesConfig:
    id: str
    units: str
    name: str


@dataclass(frozen=True)
class BLSConfig:
    base_url: str
    start_year: int
    max_years_per_request: int
    series: List[BLSSeriesConfig]
    output_path: Path


@dataclass(frozen=True)
class BEATableConfig:
    table_name: str
    line_number: int
    units: str
    name: str


@dataclass(frozen=True)
class BEAConfig:
    base_url: str
    dataset: str
    frequency: str
    tables: List[BEATableConfig]
    output_path: Path


@dataclass(frozen=True)
class AppConfig:
    bls: BLSConfig
    bea: BEAConfig
    latest_lookback_months: int


def load_config(path: Union[str, Path] = "config.yaml") -> AppConfig:
    raw = yaml.safe_load(Path(path).read_text())
    bls_raw = raw["bls"]
    bea_raw = raw["bea"]
    return AppConfig(
        bls=BLSConfig(
            base_url=bls_raw["base_url"],
            start_year=bls_raw["start_year"],
            max_years_per_request=bls_raw["max_years_per_request"],
            series=[BLSSeriesConfig(**s) for s in bls_raw["series"]],
            output_path=Path(bls_raw["output_path"]),
        ),
        bea=BEAConfig(
            base_url=bea_raw["base_url"],
            dataset=bea_raw["dataset"],
            frequency=bea_raw["frequency"],
            tables=[BEATableConfig(**t) for t in bea_raw["tables"]],
            output_path=Path(bea_raw["output_path"]),
        ),
        latest_lookback_months=raw["latest_lookback_months"],
    )
