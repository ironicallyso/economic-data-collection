# economic-data-collection — Spec

> Last updated: June 29, 2026

## Goal
A utility that collects U.S. real earnings (BLS) and personal consumption expenditures (BEA) data via official APIs, and runs a recurring year-over-year rate-of-change analysis. Provides clean macro indicators (real wage growth, PCE inflation, real consumption) as inputs to the user's broader analysis workflow.

## Requirements
1. Collect from the **BLS Public Data API v2**, monthly, seasonally adjusted, constant 1982–84 dollars, total private:
   - Real average hourly earnings, all employees — `CES0500000013`
   - Real average weekly earnings, all employees — `CES0500000012`
2. Collect from the **BEA API** (dataset NIPA, monthly), total-PCE line (line 1) of each table:
   - Real PCE level, chained dollars — Table 2.8.6 (`TableName=T20806`)
   - PCE price index — Table 2.8.4 (`TableName=T20804`)
3. Provide two collection modes per source:
   - **historical** — full backfill of all available history
   - **latest** — incremental update of the most recent data points
4. `latest` mode upserts by `(series_id, date)`, overwriting recent months to capture agency revisions. Never append-only.
5. All series IDs, table names, and line numbers are **config-driven**, not hardcoded — adding a series should require only a config edit.
6. Persist to **CSV in tidy long format, one file per source** (one BLS earnings file, one BEA PCE file), with columns `series_id, date, value, units, source, fetched_at`.
7. R analysis computes, per series: YoY rate of change (configurable: simple percent **or** log difference) at month *t* vs *t−12*, then a **3-month trailing average** of that YoY series. Before the YoY math, each series is completed to a regular monthly grid; interior gaps up to a configurable `analysis.max_fill_months` are **linearly interpolated** (so the positional 12-month lag stays valid), interpolated months are flagged (`imputed`), and an interior gap longer than the cap causes that series to be skipped with a warning.
8. R analysis produces one plot per series of the **YoY rate of change** with its **3-month moving average** overlaid, saved to disk.
9. API keys are read from environment variables; never committed.

## Out of Scope
- Scheduling (launchd / Slack trigger) — lives in a separate private layer, not this repo.
- Slack delivery of plots/results — private layer (requires a webhook secret).
- Any data source or series beyond the four above — extensible via config, but not built now.
- HTML scraping — APIs only.
- Forecasting or modeling — this repo is collection + descriptive analysis only.

## Integration Points
- **BLS Public Data API v2** — `https://api.bls.gov/publicAPI/v2/timeseries/data/`. Optional key via `BLS_API_KEY` (raises limits to 20yr/request, 500/day). Unregistered works for ≤10yr/request.
- **BEA API** — `https://apps.bea.gov/api/data/`. Key **required** via `BEA_API_KEY`. Returns the full table as JSON under `BEAAPI.Results.Data` (fields include `LineNumber`, `TimePeriod`, `DataValue`).
- **FRED API (`series/observations`)** — `https://api.stlouisfed.org/fred/series/observations`. Key **required** via `FRED_API_KEY`. Used to collect the Federal Funds Effective Rate (`DFF`). Returns JSON under `observations[]` (fields `date`, `value`); `value == "."` marks a missing/not-yet-published observation and must be dropped.

## Known Constraints
- BLS caps each request at 10 years unregistered / 20 with a key. Earnings series begin 2006-03, so a full backfill is ≤2 requests.
- BEA returns a whole table; filter to the total-PCE line (line 1) before persisting.
- FRED's `DFF` is published **daily**, unlike BLS/BEA's monthly series. It is collected into its own CSV (`outputs/fred_dff.csv`) and is intentionally **not** fed into the monthly YoY/3-month-MA analysis pipeline or combined plots, which assume a one-row-per-month grid.
- Plots: titled, axes labeled with units, legend present, minimum 10×6 inches at 150 dpi.

## Known Pitfalls
- **Do not scrape** BLS/BEA HTML pages — use the APIs.
- **Do not recompute "real" from nominal** — pull the already-deflated real series (`CES0500000013` / `...012`) and the BEA real table (`T20806`) directly.
- **Do not append-only** on `latest` pulls — BLS and BEA revise prior months; upsert and overwrite by `(series_id, date)`.
- **Do not commit API keys** — env vars only; confirm `.gitignore` excludes `.env` before the first commit.
- **Do not confuse levels with the price index** — real PCE *level* is Table 2.8.6 (`T20806`); PCE *price index* is Table 2.8.4 (`T20804`).
- **Order of operations in analysis** — compute YoY first, then the 3-month average *of the YoY series*, not a 3-month average of the level.
- **Monthly gaps misalign the positional lag** — `compute_yoy()` lags by row position (`lag(value, 12)`), valid only on a regular one-row-per-month grid. Do not feed gapped series straight into the lag. Complete each series to a monthly grid and linearly interpolate short interior gaps (`analysis.max_fill_months`); skip the series only when a gap exceeds the cap. Never silently drop months or left-align a gapped series.

## Acceptance Criteria
- Running historical collection writes **one CSV per source** (BLS, BEA) with monthly rows for its series: earnings ≥ ~230 rows each (2006→present), PCE spanning available monthly history.
- Running latest collection updates the existing CSV in place; running it twice in a row yields **no duplicate** `(series_id, date)` rows and refreshes the most recent months.
- The R analysis run produces, per series, a YoY column, a 3-month-average column, and a saved plot file; one YoY value spot-checks correctly by hand.
- A series with a short interior gap (e.g. one missing month) still produces a plot: the missing month is added with `imputed = TRUE`, its value equals the linear interpolation of the bracketing months, and it is drawn as an open marker on the plot. A series whose interior gap exceeds `analysis.max_fill_months` is skipped with a warning naming the series and gap.
- No secrets appear in committed files or `git log`.
