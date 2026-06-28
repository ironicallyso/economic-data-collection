import json
from pathlib import Path

from collect.bea import parse_bea_response

FIXTURES = Path(__file__).parent / "fixtures"


def test_parse_bea_response_filters_line_number_t20806():
    raw = json.loads((FIXTURES / "bea_t20806.json").read_text())
    rows = parse_bea_response(
        raw, line_number=1, units="billions of chained 2017 dollars, SAAR"
    )
    assert len(rows) == 2
    assert all(r["series_id"] == "DPCERX" for r in rows)


def test_parse_bea_response_strips_commas():
    raw = json.loads((FIXTURES / "bea_t20806.json").read_text())
    rows = parse_bea_response(
        raw, line_number=1, units="billions of chained 2017 dollars, SAAR"
    )
    by_date = {r["date"]: r for r in rows}
    assert by_date["2024-01-01"]["value"] == 16789.3
    assert by_date["2024-02-01"]["value"] == 16812.7
    assert isinstance(by_date["2024-01-01"]["value"], float)


def test_parse_bea_response_date_from_timeperiod():
    raw = json.loads((FIXTURES / "bea_t20806.json").read_text())
    rows = parse_bea_response(
        raw, line_number=1, units="billions of chained 2017 dollars, SAAR"
    )
    dates = {r["date"] for r in rows}
    assert dates == {"2024-01-01", "2024-02-01"}


def test_parse_bea_response_filters_line_number_t20804():
    raw = json.loads((FIXTURES / "bea_t20804.json").read_text())
    rows = parse_bea_response(raw, line_number=1, units="index 2017=100")
    assert len(rows) == 2
    assert all(r["series_id"] == "DPCERG" for r in rows)
    by_date = {r["date"]: r for r in rows}
    assert by_date["2024-01-01"]["value"] == 123.456
    assert by_date["2024-01-01"]["units"] == "index 2017=100"
    assert by_date["2024-01-01"]["source"] == "BEA"
