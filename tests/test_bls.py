import json
from pathlib import Path

from collect.bls import chunk_year_ranges, parse_bls_response

FIXTURES = Path(__file__).parent / "fixtures"


def test_chunk_year_ranges_splits_correctly():
    assert chunk_year_ranges(2006, 2026, 10) == [
        (2006, 2015),
        (2016, 2025),
        (2026, 2026),
    ]


def test_chunk_year_ranges_single_year():
    assert chunk_year_ranges(2024, 2024, 10) == [(2024, 2024)]


def test_chunk_year_ranges_exact_multiple():
    assert chunk_year_ranges(2000, 2019, 10) == [(2000, 2009), (2010, 2019)]


def test_parse_bls_response_drops_m13():
    raw = json.loads((FIXTURES / "bls_sample_response.json").read_text())
    rows = parse_bls_response(raw, {"CES0500000013": "1982-84 dollars"})
    assert len(rows) == 2
    assert all(r["date"] != "2024-13-01" for r in rows)


def test_parse_bls_response_drops_unpublished_placeholder():
    raw = json.loads((FIXTURES / "bls_sample_response.json").read_text())
    rows = parse_bls_response(raw, {"CES0500000013": "1982-84 dollars"})
    assert all(r["date"] != "2024-03-01" for r in rows)


def test_parse_bls_response_dates_and_values():
    raw = json.loads((FIXTURES / "bls_sample_response.json").read_text())
    rows = parse_bls_response(raw, {"CES0500000013": "1982-84 dollars"})
    by_date = {r["date"]: r for r in rows}
    assert set(by_date.keys()) == {"2024-01-01", "2024-02-01"}
    assert by_date["2024-01-01"]["value"] == 11.15
    assert by_date["2024-02-01"]["value"] == 11.18
    assert isinstance(by_date["2024-01-01"]["value"], float)
    assert by_date["2024-01-01"]["series_id"] == "CES0500000013"
    assert by_date["2024-01-01"]["units"] == "1982-84 dollars"
    assert by_date["2024-01-01"]["source"] == "BLS"


def test_parse_bls_response_unknown_series_units_defaults_empty():
    raw = json.loads((FIXTURES / "bls_sample_response.json").read_text())
    rows = parse_bls_response(raw, {})
    assert all(r["units"] == "" for r in rows)
