import json
from pathlib import Path

from collect.fred import parse_fred_response

FIXTURES = Path(__file__).parent / "fixtures"


def test_parse_fred_response_drops_missing_value_placeholder():
    raw = json.loads((FIXTURES / "fred_dff_sample.json").read_text())
    rows = parse_fred_response(raw, "DFF", "percent")
    assert len(rows) == 3
    assert all(r["date"] != "2024-01-01" for r in rows)


def test_parse_fred_response_dates_and_values():
    raw = json.loads((FIXTURES / "fred_dff_sample.json").read_text())
    rows = parse_fred_response(raw, "DFF", "percent")
    by_date = {r["date"]: r for r in rows}
    assert set(by_date.keys()) == {"2024-01-02", "2024-01-03", "2024-01-04"}
    assert by_date["2024-01-02"]["value"] == 5.33
    assert isinstance(by_date["2024-01-02"]["value"], float)
    assert by_date["2024-01-02"]["series_id"] == "DFF"
    assert by_date["2024-01-02"]["units"] == "percent"
    assert by_date["2024-01-02"]["source"] == "FRED"
