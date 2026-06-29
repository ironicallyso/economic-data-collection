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

## R analysis

**Phase 2 scope:** reads `outputs/bls_earnings.csv` and `outputs/bea_pce.csv` (already
populated by the Python collectors above) and computes, per series, the year-over-year
rate of change and its 3-month trailing moving average, saving one plot per series. No
API calls, no scheduling, no forecasting.

```bash
Rscript -e 'install.packages(c("tidyverse", "slider", "zoo", "yaml", "testthat"))'
Rscript analysis/run.R
Rscript tests/testthat.R
```

`analysis/run.R` writes PNGs to `outputs/plots/` and prints a spot-check tail (date,
value, yoy, yoy_ma) for one series. The YoY method (`"percent"` or `"log"`) and moving
average window are configured in `config.yaml` under `analysis:`, not hardcoded.

If a series has a gap in consecutive months (e.g. a government shutdown delays a release),
the series is completed to a regular monthly grid and short interior gaps are **linearly
interpolated** before the YoY math runs. Interpolated months are flagged (`imputed`) and
drawn as open markers on the plot. An interior gap longer than `analysis.max_fill_months`
(config) is left unfilled and that series is skipped with a warning naming the series and gap.
