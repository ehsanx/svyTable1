#' Perform Reliability Diagnostics on Survey Regression Models
#'
#' @description
#' This function takes a fitted survey regression model object (e.g., from `svyglm`)
#' and produces a tibble with key reliability and diagnostic metrics for each
#' coefficient.
#'
#' @details
#' The output provides a comprehensive overview to help assess the stability and
#' precision of each regression coefficient. The metrics include:
#' \itemize{
#'   \item \strong{Standard Error (SE)}: A measure of the estimate's precision. Smaller is better.
#'   \item \strong{p-value}: The probability of observing the data if the coefficient were zero.
#'   \item \strong{Confidence Interval (CI) Width}: A wide CI indicates greater uncertainty.
#'   \item \strong{Relative Standard Error (RSE)}: Calculated as `(SE / |Estimate|) * 100`.
#' }
#'
#' \strong{Note on RSE}: While included for comparative purposes, the use of RSE to
#' evaluate the reliability of regression coefficients is not recommended by
#' agencies like NCHS/CDC. Coefficients near zero can have an extremely large RSE
#' even if precisely estimated. It is better to rely on the standard error,
#' p-value, and confidence interval width for reliability assessment.
#'
#' @param fit A fitted model object, typically of class `svyglm`.
#' @param p_threshold A numeric value (between 0 and 1) for the significance threshold. Defaults to `0.05`.
#' @param rse_threshold A numeric value for flagging high Relative Standard Error (RSE). Defaults to `30`.
#'
#' @return
#' A `tibble` containing the following columns:
#' \itemize{
#'   \item \code{Term}: The name of the regression coefficient.
#'   \item \code{Estimate}: The coefficient's point estimate (e.g., on the log-odds scale for logistic models).
#'   \item \code{SE}: The standard error of the estimate.
#'   \item \code{p.value}: The p-value for the coefficient.
#'   \item \code{is_significant}: A logical flag, `TRUE` if `p.value` is less than `p_threshold`.
#'   \item \code{CI_Lower}: The lower bound of the 95% confidence interval.
#'   \item \code{CI_Upper}: The upper bound of the 95% confidence interval.
#'   \item \code{CI_Width}: The absolute width of the confidence interval (`CI_Upper - CI_Lower`).
#'   \item \code{RSE_percent}: The Relative Standard Error, as a percentage.
#'   \item \code{is_rse_high}: A logical flag, `TRUE` if `RSE_percent` is greater than or equal to `rse_threshold`.
#' }
#'
#' @importFrom dplyr mutate bind_cols select
#' @importFrom tibble as_tibble
#' @importFrom stats confint setNames
#'
#' @export
#'
#' @examples
#' # Ensure required packages are loaded
#' if (requireNamespace("survey", quietly = TRUE) &&
#'     requireNamespace("NHANES", quietly = TRUE) &&
#'     requireNamespace("dplyr", quietly = TRUE)) {
#'
#'   # 1. Prepare Data using the NHANES example
#'   data(NHANESraw, package = "NHANES")
#'   nhanes_adults_with_na <- NHANESraw %>%
#'     dplyr::filter(Age >= 20) %>%
#'     dplyr::mutate(
#'       ObeseStatus = factor(ifelse(BMI >= 30, "Obese", "Not Obese"),
#'                            levels = c("Not Obese", "Obese")),
#'       Race1 = factor(Race1)
#'     )
#'
#'   # Create a complete-case design object for the regression model
#'   nhanes_complete <- nhanes_adults_with_na[complete.cases(
#'     nhanes_adults_with_na[, c("ObeseStatus", "Age", "Race1")]
#'   ), ]
#'
#'   adult_design_complete <- survey::svydesign(
#'     id = ~SDMVPSU,
#'     strata = ~SDMVSTRA,
#'     weights = ~WTMEC2YR,
#'     nest = TRUE,
#'     data = nhanes_complete
#'   )
#'
#'   # 2. Fit a survey-weighted logistic regression model
#'   fit <- survey::svyglm(
#'     ObeseStatus ~ Age + Race1,
#'     design = adult_design_complete,
#'     family = quasibinomial()
#'   )
#'
#'   # 3. Get the reliability diagnostics table
#'   diagnostics_table <- svyglmdiag(fit)
#'
#'   # Print the resulting table
#'   print(diagnostics_table)
#'
#'   # For a publication-ready table, pipe the result to kable()
#'   if (requireNamespace("knitr", quietly = TRUE)) {
#'     knitr::kable(diagnostics_table,
#'                  caption = "Reliability Diagnostics for NHANES Obesity Model",
#'                  digits = 3)
#'   }
#' }

svyglmdiag <- function(fit, p_threshold = 0.05, rse_threshold = 30) {

  # --- Input validation ---
  if (!inherits(fit, "svyglm")) {
    warning("This function is designed for 'svyglm' objects. Results may be unexpected.")
  }

  # 1. Get the standard model summary and confidence intervals
  summary_fit <- summary(fit)
  conf_int_fit <- stats::confint(fit)

  # 2. Combine these into a single, informative table
  reliability_df <- tibble::as_tibble(summary_fit$coefficients, rownames = "Term")
  names(reliability_df) <- c("Term", "Estimate", "SE", "t.value", "p.value")

  # 3. Add CIs, calculate metrics, and add flags
  reliability_df <- reliability_df %>%
    dplyr::bind_cols(tibble::as_tibble(conf_int_fit) %>%
                       stats::setNames(c("CI_Lower", "CI_Upper"))) %>%
    dplyr::mutate(
      RSE_percent = (SE / abs(Estimate)) * 100,
      CI_Width = CI_Upper - CI_Lower,
      is_significant = p.value < p_threshold,
      is_rse_high = RSE_percent >= rse_threshold
    ) %>%
    # Reorder and select the final columns for a clean output
    dplyr::select(
      Term,
      Estimate,
      SE,
      p.value,
      is_significant,
      CI_Lower,
      CI_Upper,
      CI_Width,
      RSE_percent,
      is_rse_high
    )

  return(reliability_df)
}
