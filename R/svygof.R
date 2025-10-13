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
#' A `data.frame` containing the F-statistic, the numerator (df1) and
#' denominator (df2) degrees of freedom, and the p-value for the test.
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
#' \dontrun{
#' # Ensure required packages are loaded
#' if (requireNamespace("survey", quietly = TRUE) &&
#'     requireNamespace("NHANES", quietly = TRUE) &&
#'     requireNamespace("dplyr", quietly = TRUE)) {
#'
#'   # 1. Prepare Data
#'   data(NHANESraw, package = "NHANES")
#'   nhanes_data <- NHANESraw %>%
#'     dplyr::filter(Age >= 20) %>%
#'     dplyr::mutate(ObeseStatus = factor(ifelse(BMI >= 30, "Obese", "Not Obese"),
#'                                        levels = c("Not Obese", "Obese"))) %>%
#'     dplyr::filter(complete.cases(ObeseStatus, Age, Gender, Race1,
#'                                  WTMEC2YR, SDMVPSU, SDMVSTRA))
#'
#'   # 2. Create a replicate design object
#'   std_design <- survey::svydesign(
#'     ids = ~SDMVPSU,
#'     strata = ~SDMVSTRA,
#'     weights = ~WTMEC2YR,
#'     nest = TRUE,
#'     data = nhanes_data
#'   )
#'   rep_design <- survey::as.svrepdesign(std_design)
#'
#'   # 3. Fit a survey logistic regression model using the replicate design
#'   fit_obesity_rep <- survey::svyglm(
#'     ObeseStatus ~ Age + Gender + Race1,
#'     design = rep_design,
#'     family = quasibinomial()
#'   )
#'
#'   # 4. Calculate the design-correct AUC
#'   auc_results <- svyAUC(fit_obesity_rep, rep_design)
#'   print(auc_results)
#' }
#' }
svygof <- function(fit, design, G = 10) {

  # Get residuals and fitted values from the model
  resids <- stats::residuals(fit, type = "response")
  fitted_vals <- stats::fitted(fit)

  # Create a data frame of model results, using row names to link back
  model_data <- data.frame(
    .id = names(resids),
    r = resids,
    f = fitted_vals
  )

  # Use the data directly from the design object, which is the most reliable source
  data_with_res <- design$variables
  data_with_res$.id <- rownames(data_with_res)
  data_with_res <- merge(data_with_res, model_data, by = ".id", all.x = TRUE)

  # Create G groups based on fitted values
  breaks <- stats::quantile(data_with_res$f, probs = seq(0, 1, 1 / G), na.rm = TRUE)
  unique_breaks <- unique(breaks)
  data_with_res$g <- cut(data_with_res$f, breaks = unique_breaks, include.lowest = TRUE)

  # Rebuild the design object using its internal components
  new_design <- survey::svydesign(
    ids = design$cluster,
    strata = design$strata,
    weights = design$weights,
    data = data_with_res,
    nest = isTRUE(design$nest)
  )

  # Run the test
  decile_model <- survey::svyglm(r ~ g, design = new_design, na.action = na.omit)
  test_result <- survey::regTermTest(decile_model, ~g)

  # Return a tidy data frame
  output <- data.frame(
    F_statistic = test_result$Ftest[1],
    df1 = test_result$df,
    df2 = test_result$ddf,
    p_value = test_result$p
  )

  return(output)
}
