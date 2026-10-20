# AGENTS.md: cellsynth

Package that turns de-identified tables into synthetic counterparts (same
layout, real identifiers, redrawn values). Package root = repo root. R only.
Updated 2026-10-20.

## Data handling (read first)

- `1_input_databases/`, `2_synthetic/` and `private/` are git-ignored and
  never committed. `private/` holds the client-specific layer (run scripts,
  wrangling, reviewed specs, client journal) and may exist only on the
  maintainer's machine.
- Never paste rows, values, identifiers or column schemas of client data into
  prompts for external models; send package code and toy data only.
- Synthetic outputs are not anonymised: identifiers are real and text columns
  are permuted real strings.

## File roles

| File | Role |
|---|---|
| `R/read_source.r` | `read_source()`: xlsx/csv read as raw character; original header and readxl-guessed types kept; CSV delimiter/encoding detection; `source_sheets()` |
| `R/parse.r` | number/date parsers and label normalisation, used for modelling only |
| `R/spec.r` | `infer_spec()` role heuristics, `write_spec()`/`read_spec()`, `detect_nonsubject_rows()` |
| `R/synthesize.r` | the engine: `synthesize_table()`, `leaf_donors()`, block-wise screening, lock groups, `make_aux()`, `passthrough_result()` |
| `R/write_output.r` | `write_synthetic()`: one table or a workbook, original header, sheet order and cell types |
| `R/compare.r` | `compare_tables()`, `row_disclosure()`, `compare_correlations()` |
| `R/pipeline.r` | `synthesize_file()`: every sheet, per-sheet specs, small-sheet policy, m copies, run log |
| `inst/functions/load_stuff.r` | template loader, sources `R/` |
| `tests/testthat/` | 97 tests on toy data (helper-toy.R, helper-workbook.R); `SYNTH_SKIP_SLOW=1` skips the ~2 min wide-sheet test |

## Engine in brief

Per column, 10 random folds; per round, censored cells are redrawn column by
column (random order) by copying a donor's raw string from its rpart leaf
(minbucket 5, cp 1e-4, 25 screened predictors). Two stages for
numeric/date/text: cell state (value/missing/junk), then value. Text values
are random draws from the observed pool. `censor_mode = "cell"` (default)
conditions on the row's other current values; lock groups (screening
association >= 0.95) share a fold and cannot see each other's censored cells.
`"row"` censors whole rows and rebuilds them from already redrawn columns and
aux only. Declared aux columns are always offered as predictors. JOURNAL.md
has the reasons (R1, R2).

## Workbooks

`synthesize_file()` takes every sheet by default and writes one workbook;
specs per sheet as `<stem>__<NN>_<sheet>_spec.csv` (NN = workbook position)
when the workbook has more than one sheet, `<stem>_spec.csv` otherwise.
Sheets go through `passthrough_result()` when empty / no modelled columns
(copy) or under `4 * minbucket` subject rows (`small_sheets`, default blank).
`results` is a list over copies of lists over sheets.

## Running

- Tests: `devtools::test()` from a temp `.R` file (never multi-line
  `Rscript -e`, it segfaults intermittently on the maintainer's machine).
- Never edit a script while an Rscript process is running it: Rscript reads
  the file as a stream and parses the edited bytes after the current
  top-level expression.
- `R CMD check` on the maintainer's machine: R's own system profile
  (`R-4.6.0/library/base/R/Rprofile`, line ~226) hard-codes
  `.libPaths(".../_Libraries")`, which replaces the library `R CMD check`
  sets up for its child processes. A check therefore loads whatever is
  installed in `_Libraries`, not the build. Procedure: `R CMD INSTALL .`
  into `_Libraries` first, then check (the check then tests the same code).
  If the install says "cannot remove earlier installation" (OneDrive
  handles), rename the old folder aside, install, delete it. Pandoc for the
  README/NEWS check: `RSTUDIO_PANDOC=".../RStudio/resources/app/bin/quarto/bin/tools"`.
- CRAN mode: `skip_on_cran()` on the three wide-table tests; devtools
  sets `NOT_CRAN=true`, so time CRAN mode with `NOT_CRAN=false` (~70 s).
- Runtime guide: 900 x 100 about 1 min; 1,000 x 1,100 about 5-10 min;
  900 x 9,000 about 50-70 min, about 0.5 GB RAM.
