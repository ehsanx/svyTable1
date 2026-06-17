# svyTable1 0.14.1.9000

## New features

* `svydesign_build()` builds a complex-survey design from plain column names with
  input validation and clear error messages, and supports subpopulation
  ("domain") analysis the correct way -- by subsetting the design rather than
  pre-filtering rows, which would break subgroup standard errors.
* `addint()` and `addintlist()` gain `ci_method = c("delta", "mover")`. The new
  `"mover"` option uses the MOVER interval of Zou (2008) <doi:10.1093/aje/kwn104>
  for RERI, which generally has better coverage than the symmetric Wald
  interval. The default (`"delta"`) is unchanged.
* `addintlist()` now returns a `Scale` column ("OR (logistic)", "HR (Cox)",
  "RR (log-binomial)", "RR (Poisson)", or "ratio") that makes the effect-measure
  scale of RERI/AP/S explicit.
* `svykmplot()` gains an `se` argument. `se = FALSE` draws the survival curves
  without the (slow) survey-weighted confidence bands.
* New "Getting Started" vignette giving an end-to-end SPPH 604 workflow, and a
  package-level help page (`?svyTable1`) indexing the functions by purpose.

## Improvements and bug fixes

* `svygof()` now attaches the decile groups to the existing design via
  `update()` instead of rebuilding a bare `svydesign`, so it preserves finite
  population corrections and calibration and works on replicate-weights designs
  (it previously errored on them).
* `svyAUC()` builds its confidence interval on the logit scale (so it stays
  within `[0, 1]`) and uses the design degrees of freedom instead of a fixed
  critical value; removed redundant internal computation.
* `svycoxph_CE_mi()` default `time_var`/`status_var` corrected to
  `"stime"`/`"status"`.
* `svytable1()` now errors on a zero-row design instead of returning a
  placeholder table, and no longer over-suppresses cells whose design effect is
  below 1.
* Documentation: method references with DOIs added to `DESCRIPTION`; every
  exported function's `\value` now names its output class and meaning; the
  additive-interaction help notes that RERI/AP/S approximate risk-ratio measures
  only when the outcome is rare.
* Examples and vignettes are now self-contained on the bundled `nhanes_mortality`
  data and no longer require the `NHANES` package.
* Added a real test suite (replacing placeholder smoke tests).
