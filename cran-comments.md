## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

* local Windows 11, R 4.6.0 (`R CMD check --as-cran`, PDF manual built)
* R-hub v2 (GitHub Actions): linux (R-devel), windows (R-devel),
  macos-arm64 (R-devel), ubuntu-release, ubuntu-next: all Status OK
* win-builder: R-devel and R-release (submitted 2026-10-20)

## Notes

* Tests that take more than a few seconds (wide-table screening and
  synthesis) are skipped on CRAN; the CRAN-mode test run takes about 70 s.
* Examples write only to `tempdir()`.
