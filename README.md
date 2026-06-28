# economic-data-collection

Collects U.S. real earnings (BLS) and personal consumption expenditures (BEA) via
official government APIs and stores them as tidy CSVs. See [SPEC.md](SPEC.md) for
full requirements; see [CLAUDE.md](CLAUDE.md) for conventions.

**Phase 1 scope:** collection layer only. BLS works live and unregistered for our
2006-present range. BEA is implemented but requires `BEA_API_KEY` to run live — until
a key is available, BEA parsing is verified against fixtures in `tests/`.

## Setup

```bash
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
cp .env.example .env   # fill in BLS_API_KEY (optional) / BEA_API_KEY (required for BEA)
```

## Usage

```bash
python -m collect --source bls --mode historical   # full backfill, overwrites outputs/bls_earnings.csv
python -m collect --source bls --mode latest        # incremental upsert of recent months
python -m collect --source bea --mode historical    # requires BEA_API_KEY in environment
python -m collect --source bea --mode latest         # requires BEA_API_KEY in environment
```

## Testing

```bash
pytest
black --check collect tests
```

All tests run offline against saved fixtures in `tests/fixtures/` — no network calls.
