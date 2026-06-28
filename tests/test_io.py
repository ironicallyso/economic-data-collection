import pandas as pd

from collect.io import upsert_csv


def _row(series_id, date, value):
    return {
        "series_id": series_id,
        "date": date,
        "value": value,
        "units": "1982-84 dollars",
        "source": "BLS",
    }


def test_upsert_creates_new_file(tmp_path):
    output_path = tmp_path / "out.csv"
    rows = [_row("A", "2024-01-01", 1.0), _row("A", "2024-02-01", 2.0)]
    upsert_csv(rows, output_path)

    df = pd.read_csv(output_path, dtype={"series_id": str})
    assert list(df.columns) == [
        "series_id",
        "date",
        "value",
        "units",
        "source",
        "fetched_at",
    ]
    assert len(df) == 2


def test_upsert_merges_without_duplicates(tmp_path):
    output_path = tmp_path / "out.csv"
    upsert_csv(
        [_row("A", "2024-01-01", 1.0), _row("A", "2024-02-01", 2.0)], output_path
    )
    upsert_csv(
        [_row("A", "2024-02-01", 99.0), _row("A", "2024-03-01", 3.0)], output_path
    )

    df = pd.read_csv(output_path, dtype={"series_id": str})
    assert len(df) == 3
    assert not df.duplicated(subset=["series_id", "date"]).any()

    by_date = df.set_index("date")["value"]
    assert by_date["2024-01-01"] == 1.0
    assert by_date["2024-02-01"] == 99.0
    assert by_date["2024-03-01"] == 3.0


def test_upsert_sets_fetched_at(tmp_path):
    output_path = tmp_path / "out.csv"
    upsert_csv([_row("A", "2024-01-01", 1.0)], output_path)

    df = pd.read_csv(output_path)
    assert "fetched_at" in df.columns
    assert pd.notna(df.loc[0, "fetched_at"])
    pd.Timestamp(df.loc[0, "fetched_at"])  # parses without raising
