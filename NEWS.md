# HLCtools 0.1.0

## CRAN submission amendments

* Added DOI-formatted references to related behavioural synchrony
  methodologies in the package Description.
* Clarified that the specific Herd Lying Concordance formulations are
  introduced in this package.
* Removed unnecessary `\dontrun{}` wrappers from short, executable examples.
* Ensured that all exported functions have directly executable examples and
  installed help pages.

## New features

* Added `calculate_hlc()` for interval-level Herd Lying Concordance calculations.
* Added SD-, MAD-, IQR-, and entropy-based HLC implementations.
* Added lying-weighted HLC metrics.
* Added threshold synchrony indicators.
* Added `summarise_hlc_daily()` for daily group-level summaries.
* Added `compare_hlc_methods()` for descriptive comparison of HLC variants.
* Added `rank_hlc_methods()` for dataset-specific descriptive ranking of HLC variants.
* Added `hlc_methods()` and `hlc_weighted_methods()`.
* Added `example_lies` example dataset.
* Added getting-started vignette.
