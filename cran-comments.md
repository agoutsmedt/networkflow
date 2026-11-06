## Release summary

This is the first submission of networkflow (version 0.1.0).

The package imports biblionetwork (>= 0.2.0), which is on CRAN since
2026-11-04. Packages used only in examples, vignettes-like articles or
optional features (backbone, duckdb, DBI, ggalluvial, uwot, textstem,
tidytext, scico, lifecycle) are in Suggests and are used conditionally (`requireNamespace()` or `rlang::check_installed()` in the code, `requireNamespace()` in examples, `skip_if_not_installed()` in tests).

## Test environments

* Local: Fedora, R 4.6.1
* GitHub Actions (`R-CMD-check`): macOS (R release), Windows (R release),
  Ubuntu (R release and oldrel-1)

## R CMD check results

0 errors | 0 warnings | 0 notes on all the environments above.

* This is a new submission, so CRAN incoming checks will report
  "New submission".

## Downstream dependencies

None: this is a new package.
