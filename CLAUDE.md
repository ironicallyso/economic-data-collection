# economic-data-collection

## Project Overview
Public utility that collects U.S. real earnings (BLS) and personal consumption expenditures (BEA) via official government APIs, stores them as CSVs, and runs a recurring year-over-year rate-of-change analysis in R. Collection and analysis only — no scheduling, no forecasting.

## Tech Stack
- **Collection:** Python 3.11+ — `requests` (HTTP), `pandas` (CSV read/write, upsert by key). No web framework.
- **Analysis:** R 4.x — tidyverse (`readr`, `dplyr`, `ggplot2`, `lubridate`) + `slider` for the moving average, `yaml` for config, `testthat` for tests.
- **Config:** `config.yaml` (series IDs, BEA table names + line numbers, output paths, lookback) read by both languages. No values hardcoded in source.
- **Secrets:** `.env` (gitignored) — `BEA_API_KEY` (required), `BLS_API_KEY` (optional).

## Dev Workflow
Greenfield; finalize exact commands during implementation. Intended:
- Setup: `python -m venv .venv && source .venv/bin/activate && pip install -r requirements.txt`
- Collect: `python -m collect --source {bls|bea} --mode {historical|latest}`
- Analyze: `Rscript analysis/run.R`
- Tests: `pytest` — collectors tested against saved API-response fixtures, not live calls.

## Coding Conventions
- Python: PEP 8, type hints on public functions, `black` formatting.
- R: tidyverse style.
- Every tunable (series, tables, paths, lookback window) comes from `config.yaml` — no magic constants in code.

## Key Rules
- Read API keys from the environment (`.env`); never hardcode or commit secrets. Confirm `.env` and the data output dir are gitignored **before the first commit**.
- Data files and plots are build artifacts → write to a gitignored `outputs/` dir; never commit data.
- Keep collection in Python and analysis in R — do not reimplement one in the other.
- One feature/experiment per PR; small, attributable commits.
- Data-source specifics and pitfalls (agency revisions, real-vs-nominal, table/line IDs, YoY-then-average order) live in `SPEC.md` — follow them; do not contradict them.

## Project Spec
At the start of each session, read `SPEC.md` to understand current requirements before planning or writing code.

## Agent Behavior
- Confirm before destructive operations (deleting files, `git` force/reset, history rewrites).
- Confirm before pushing to the public GitHub remote.
- Prefer editing existing files over creating new ones.
