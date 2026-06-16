#' Perform Reliability Diagnostics on Survey Regression Models
#'
#' @description
#' This function takes a fitted survey regression model object (e.g., from `svyglm`
#' or `svycoxph`) and produces a tibble with key reliability and diagnostic
#' metrics for each coefficient.
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
#' @param fit A fitted model object from the `survey` package, such as `svyglm` or `svycoxph`.
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
#' @importFrom dplyr mutate select
#' @importFrom tibble tibble
#' @importFrom stats confint coef vcov
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' data(nhanes_mortality, package = "svyTable1")
#' nhanes_mortality$htn01 <- as.numeric(nhanes_mortality$htn == "Yes")
#'
#' design <- survey::svydesign(
#'   id = ~psu, strata = ~strata, weights = ~survey_weight,
#'   nest = TRUE, data = nhanes_mortality
#' )
#'
#' fit <- survey::svyglm(htn01 ~ age + sex + race,
#'                       design = design, family = quasibinomial())
#'
#' # Per-coefficient reliability diagnostics (SE, p-value, CI width, RSE).
#' svydiag(fit)

svydiag <- function(fit, p_threshold = 0.05, rse_threshold = 30) {

  # 1. Robustly extract key model components using accessor functions
  s_fit <- summary(fit)
  estimates <- stats::coef(fit)
  se <- sqrt(diag(stats::vcov(fit)))
  conf_int <- stats::confint(fit)

  # P-values are most reliably extracted from the summary coefficient table.
  # This assumes the p-value is the last column, which is standard for most
  # survey models (svyglm, svycoxph, etc.).
  p_vals <- s_fit$coefficients[, ncol(s_fit$coefficients)]

  # 2. Combine these into a single, informative table
  reliability_df <- tibble::tibble(
    Term = names(estimates),
    Estimate = estimates,
    SE = se,
    p.value = p_vals,
    CI_Lower = conf_int[, 1],
    CI_Upper = conf_int[, 2]
  )

  # 3. Calculate derived metrics, add flags, and finalize the output
  reliability_df <- reliability_df %>%
    dplyr::mutate(
      RSE_percent = (.data$SE / abs(.data$Estimate)) * 100,
      CI_Width = .data$CI_Upper - .data$CI_Lower,
      is_significant = .data$p.value < p_threshold,
      is_rse_high = .data$RSE_percent >= rse_threshold
    ) %>%
    # Reorder and select the final columns for a clean output
    dplyr::select(
      "Term",
      "Estimate",
      "SE",
      "p.value",
      "is_significant",
      "CI_Lower",
      "CI_Upper",
      "CI_Width",
      "RSE_percent",
      "is_rse_high"
    )

  return(reliability_df)
}
