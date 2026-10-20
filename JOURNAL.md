# JOURNAL: SyntheticData (public)

Design decisions and rollbacks of the package. The client-specific journal
(data, validation numbers) is kept outside the repository.

## 2026-10-19: v1

**Brief.** Turn any de-identified table into a synthetic counterpart that can
leave the machine: censor 10% of each table, re-impute from the rest, repeat
until every value is imputed. Keep identifiers so tables stay joinable.
Text: permute for now. Preserve the data-quality spirit (significant digits,
numeric codes, typos).

**Decisions.**
- D1. Missingness is synthesised, not kept as a mask.
- D2. Text columns are drawn from the observed pool (permutation); proper
  text handling is open.
- D3. Donor copying instead of generating values: a censored cell takes the
  raw string of a random row in its CART leaf (as mice "cart" and synthpop).
  Formats and quirks survive for free; each value comes from a pool of at
  least `minbucket` (5) donors.
- D4. Two stages for numeric/date/text: state tree (value / missing / junk,
  junk = a cell that does not parse under the column's role), then value tree.
- D5. Predictor screening once on the original, 25 per target, by the largest
  absolute correlation among value, missingness and level signals.
- D6. Non-subject rows (totals, site headers) are detected, never donors,
  blanked except identifiers.
- D7. Output cell types restored per column from readxl's guess; header
  written as on disk, duplicates included.

**Rollbacks.**
- R1. The first version censored fold k in all columns at once, predictors
  included (the literal reading of "censor 10% of the table"). It leaked:
  training rows with a censored predictor were routed by surrogate splits into
  the wrong leaf and donated there, breaking flag/date couplings. Fix:
  censor only the target; predictors are complete (original or already
  redrawn this round, columns in random order).
- R2. That exposed the opposite problem: two columns that determine each
  other predict each other perfectly and never change, so an event flag stays
  real per identifier. Fix: lock groups (screening association >= 0.95)
  share a fold and cannot see each other's censored cells. Added
  `censor_mode = "row"` as the fully synthetic end of the dial.
- Reviews: an external model review found rpart's default `na.rpart`
  dropping all-NA-predictor rows (donors misaligned with leaves; fixed with
  `na.pass`), plus several edge cases; a toy-data test suite was written
  alongside.

**Validation (confidential data, summary only).** Joined three tables,
fitted a negative binomial and a logistic model on the original and on five
synthetic copies per scenario, pooled with the partially synthetic rule
(Reiter 2003). Count model: mean absolute standardised bias about 0.6 SE
with `aux`, about 1 SE without, about 1.8 SE in row mode. Sparse binary
outcome derived from many slot columns: about 2 SE in every scenario
(not solved). Declared aux columns are therefore always offered as
predictors (`aux_always = TRUE`).

## 2026-10-19: any workbook

- D8. `synthesize_file()` takes every sheet, writes one workbook in order,
  one spec per sheet named with its workbook position.
- D9. `passthrough_result()` for empty / no-model sheets (copied) and sheets
  under `4 * minbucket` subject rows (`small_sheets`, default blank).
- D10. Block-wise screening keeps only the top candidates and lock pairs per
  target, so 9,000-column sheets fit in about 0.5 GB.
- D11. CSV delimiter detection outside quotes; encoding detection with a
  UTF-8 validity check; ISO-8859-2 is read as windows-1250 (same Central
  European letters, plus the "±" and typographic quotes Excel writes).
- Writer fixes found by a stress run on 13 real files: openxlsx writes "\n"
  as "\r\n" (Excel's "\r\n" is collapsed first) and shifts Date serials below
  61 by a day (such columns are written as POSIXct).

## Open problems / TODO

- Sparse binary outcomes derived from many columns: derived outcome columns
  before synthesis, or joint synthesis of the joined analysis frame.
- Text handling beyond permutation.
- Derived columns are not recomputed (totals, BMI).
- Date ordering within a row is only approximately kept; relative-date
  modelling would fix it.
- Very small tables (tens of rows) push most columns into lock groups, so
  they behave close to row mode.
- 1904 Excel date system not detected.
