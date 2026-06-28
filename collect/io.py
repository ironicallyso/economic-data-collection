from __future__ import annotations

from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, List, Union

import pandas as pd

TIDY_COLUMNS = ["series_id", "date", "value", "units", "source", "fetched_at"]


def upsert_csv(rows: List[Dict], output_path: Union[str, Path]) -> None:
    """Upsert tidy rows into a CSV keyed on (series_id, date), new rows winning."""
    output_path = Path(output_path)
    fetched_at = datetime.now(timezone.utc).isoformat()

    new_df = pd.DataFrame(rows)
    new_df["fetched_at"] = fetched_at
    new_df = new_df[TIDY_COLUMNS]

    output_path.parent.mkdir(parents=True, exist_ok=True)

    if output_path.exists():
        existing_df = pd.read_csv(output_path, dtype={"series_id": str})
        combined = pd.concat([existing_df, new_df], ignore_index=True)
    else:
        combined = new_df

    combined = combined.drop_duplicates(subset=["series_id", "date"], keep="last")
    combined = combined.sort_values(["series_id", "date"]).reset_index(drop=True)
    combined.to_csv(output_path, index=False)
