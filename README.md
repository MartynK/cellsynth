# SyntheticData

Turns a de-identified table (xlsx workbook or csv) into a synthetic
counterpart with the same layout, the same identifiers and the same data
quirks, but with every other cell redrawn from the rest of the table. Meant
for developing and debugging analysis code on data you may not move off your
machine.

## Quick start

```r
# install.packages("remotes"); remotes::install_github("MartynK/SyntheticData")
library(SyntheticData)

synthesize_file("path/to/table.xlsx", out_dir = "synthetic", m = 1)
```

Every sheet of a workbook is synthesised and written into one output
workbook in the original order (`sheet =` picks a subset). The first run
writes one **spec** per sheet next to the output: one row per column with its
role (`id`, `numeric`, `date`, `categorical`, `text`, `constant`, `empty`)
and why the heuristics chose it. Fix any role they got wrong (an identifier
they missed, free text taken for a category) and run again with the same
`spec_dir`; reviewed specs are reused.

A toy run:

```r
set.seed(1)
n <- 300
toy <- data.frame(
  id   = sprintf("AB%03d", 1:n),
  age  = round(rnorm(n, 65, 10)),
  sex  = sample(c("M", "F"), n, TRUE),
  dm   = sample(c("0", "1", "1.0", NA), n, TRUE),
  ldl  = sprintf("%.2f", rlnorm(n, 1, 0.3)),
  stringsAsFactors = FALSE)
path <- file.path(tempdir(), "toy.csv")
write.csv(toy, path, row.names = FALSE, na = "")
run <- synthesize_file(path, out_dir = tempdir())
head(read.csv(run$files[1]))
```

## How it works

Each column is split into 10 random folds. Fold by fold, the censored 10% of
a column is redrawn from a CART tree fitted on the other 90%: the cell copies
the **raw string** of a random donor in its leaf (at least 5 rows), so
formats such as `1.0`, Excel date serials, decimal commas and typos survive.
Missingness is synthesised too (a first tree draws value / missing / junk).
After 10 folds every cell has been redrawn once; `n_cycles` repeats the
sweep.

- `censor_mode = "cell"` (default): a cell is redrawn given the rest of its
  own row. Associations survive well; each synthetic row is a noisy copy of
  its original, never identical to it.
- Columns that determine each other (an event flag and its date) form lock
  groups that are redrawn together, so they can change at all.
- `censor_mode = "row"`: whole rows are rebuilt from already redrawn columns
  only (fully synthetic rows, weaker associations).
- `aux =` (see `make_aux()`): columns of another table, joined by
  identifier, condition the synthesis so cross-table associations survive a
  later join. Without them, effects across tables attenuate.
- Non-subject rows (`TOTAL`, site headers), empty sheets and sheets too small
  to synthesise are detected and handled (`small_sheets` = blank / copy /
  drop).

`compare_tables()`, `compare_correlations()` and `row_disclosure()` report
fidelity and how close synthetic rows stay to their originals.

## What it does NOT do

- **Identifiers are kept as they are** (that is what keeps tables joinable).
- **Text columns are permuted real strings**: free text, including any names
  in it, is not anonymised.
- In cell mode each synthetic row depends on its own original row.

So the output is synthetic in values, **not anonymised**. Treat it
accordingly.

Other limits: the first row is always the header; formulas come out as
values; formatting and merged cells are not kept; `.xls` is written as
`.xlsx`; derived columns (a total of other columns, BMI from weight and
height) are synthesised like any other column and are not recomputed.

## Status

Validated on a confidential clinical registry (several joined tables, up to
9,000 columns per sheet): a count-outcome regression on joined synthetic
copies landed within 1.5 SE of the original on every term when the
cross-table columns were declared as `aux`; a sparse binary outcome derived
from many columns did not. Use the copies to build and debug code and for
ballpark answers; run final numbers on the real data. Open problems:
`JOURNAL.md`.
