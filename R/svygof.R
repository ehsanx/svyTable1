#' Archer-Lemeshow Goodness-of-Fit Test for Survey Models
#'
#' @description
#' Performs an Archer-Lemeshow goodness-of-fit (GOF) test for logistic
#' regression models fitted with complex survey data. This test is an extension
#' of the Hosmer-Lemeshow test for survey designs.
#'
#' @details
#' The function automates the process of calculating residuals and fitted values,
#' creating groups (deciles by default) based on fitted probabilities,
#' building a new survey design with these variables, and running a final
#' Wald test. A non-significant p-value (e.g., p > 0.05) suggests no evidence
#' of a poor fit.
#'
#' @param fit A fitted model object of class `svyglm`.
#' @param design A survey design object of class `svydesign` or `svyrep.design`
#'   that was used to fit the model.
#' @param G An integer specifying the number of groups to create based on
#'   fitted probabilities. Defaults to 10 (deciles).
#'
#' @return
#' A one-row `data.frame` with columns `F_statistic`, `df1` (numerator df),
#' `df2` (denominator df) and `p_value` for the Archer-Lemeshow design-based
#' goodness-of-fit test. A non-significant p-value (e.g. p > 0.05) is consistent
#' with adequate fit; it does not prove the model is correctly specified.
#'
#' @source
#' The implementation is a formalized function based on the script and discussion
#' in the R-help mailing list archives: \url{https://stat.ethz.ch/pipermail/r-help/2016-November/443223.html}
#'
#' @importFrom survey svydesign svyglm regTermTest
#' @importFrom stats residuals fitted quantile
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(nhanes_mortality, package = "svyTable1")
#' nhanes_mortality$htn01 <- as.numeric(nhanes_mortality$htn == "Yes")
#'
#' # The Archer-Lemeshow test uses a standard (non-replicate) survey design.
#' design <- survey::svydesign(
#'   id = ~psu, strata = ~strata, weights = ~survey_weight,
#'   nest = TRUE, data = nhanes_mortality
#' )
#'
#' fit <- survey::svyglm(htn01 ~ age + sex + smoking,
#'                       design = design, family = quasibinomial())
#'
#' # A non-significant p-value is consistent with adequate fit (it does not
#' # prove the model is correctly specified).
#' svygof(fit, design, G = 10)
#' }
svygof <- function(fit, design, G = 10) {

  if (!inherits(fit, "svyglm")) {
    stop("`fit` must be a survey logistic model of class 'svyglm'.", call. = FALSE)
  }
  if (!inherits(design, c("survey.design", "svyrep.design"))) {
    stop("`design` must be a survey design ('survey.design' or 'svyrep.design').",
         call. = FALSE)
  }

  # Get response residuals and fitted probabilities from the model.
  resids <- stats::residuals(fit, type = "response")
  fitted_vals <- stats::fitted(fit)

  # Align residuals/fitted to ALL rows of the design by row name, so that the
  # decile grouping can be attached to the *existing* design object. Rows that
  # were dropped while fitting (e.g. missing covariates) are left as NA and are
  # excluded by na.omit when the grouped model is fitted.
  rn <- rownames(design$variables)
  n  <- length(rn)
  r_full <- rep(NA_real_, n)
  f_full <- rep(NA_real_, n)
  r_full[match(names(resids), rn)] <- as.numeric(resids)
  f_full[match(names(fitted_vals), rn)] <- as.numeric(fitted_vals)

  # Create G groups based on quantiles of the fitted probabilities.
  breaks <- stats::quantile(f_full, probs = seq(0, 1, 1 / G), na.rm = TRUE)
  unique_breaks <- unique(breaks)
  if (length(unique_breaks) < 3) {
    stop("Too few distinct fitted-probability groups for the goodness-of-fit ",
         "test (the model's predictions are nearly constant). Try a smaller G.",
         call. = FALSE)
  }
  g_full <- cut(f_full, breaks = unique_breaks, include.lowest = TRUE)

  # Attach the residual and grouping variables to the EXISTING design via
  # survey::update(). Unlike rebuilding the design from $cluster/$strata/$weights,
  # this preserves finite population corrections, calibration, and replicate
  # weights, so the Archer-Lemeshow test remains design-correct -- and it works
  # for both 'survey.design' and 'svyrep.design' objects.
  design <- stats::update(design, .gof_resid = r_full, .gof_group = g_full)

  decile_model <- survey::svyglm(.gof_resid ~ .gof_group, design = design,
                                 na.action = stats::na.omit)
  test_result <- survey::regTermTest(decile_model, ~.gof_group)

  data.frame(
    F_statistic = as.numeric(test_result$Ftest[1]),
    df1 = test_result$df,
    df2 = test_result$ddf,
    p_value = as.numeric(test_result$p)
  )
}
