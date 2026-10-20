## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

* local Windows 11, R 4.6.0 (`R CMD check --as-cran`, manual built separately)
* still to run before submission: win-builder (R-devel), R-hub (Linux, macOS)

## Notes

* Tests that take more than a few seconds (wide-table screening and
  synthesis) are skipped on CRAN; the CRAN-mode test run takes about 70 s.
* Examples write only to `tempdir()`.
