# Verification on real data

Scripts used on 2026-11-04 to check `networkflow` on samples of the authors'
Scopus and Web of Science databases (`~/data/scopus/scopus.duckdb`,
`~/data/wos/wos.duckdb`), which cannot be distributed. The samples are cached in
`tools::R_user_dir("networkflow", "cache")`. This folder is excluded from the
package build (`bench/` in `.Rbuildignore`).

Run from this folder, with the package installed:

1. `Rscript 01_extract_samples.R`: Journal of Monetary Economics (Scopus, all
   years), 30,000 random Scopus documents, 30,000 Web of Science economics
   articles (1990-2004), with their references and authors.
2. `Rscript 02_inputs_and_errors.R`: wrong or unusual inputs (missing columns,
   NA, factors, integer identifiers, a placeholder identifier, thresholds,
   time windows...). Read the messages.
3. `Rscript 03_pipeline.R jme|s30|wos`: every step of the pipeline with its
   options, with the time and the maximum of R's heap of each step.
4. `Rscript 04_edge_cases.R`: empty windows through the whole pipeline, and
   errors of the other functions.

`00_helpers.R` has `run()`, which times a step and prints its messages,
warnings and errors. The findings and what was kept for later are in the
private `to_do.qmd`.
