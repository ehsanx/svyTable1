#' Calculate a Design-Correct AUC for a Survey Model
#'
#' @description
#' This function calculates the Area Under the Curve (AUC) and its design-correct
#' standard error and 95% confidence interval for a survey logistic regression
#' model. It correctly accounts for strata and clusters by using a
#' replicate-weights survey design object.
#'
#' @param fit A fitted model object of class `svyglm`.
#' @param design A replicate-weights survey design object, typically created with `as.svrepdesign`.
#'
#' @return
#' A `data.frame` containing the AUC point estimate, its standard error (SE),
#' and the lower and upper bounds of the 95% confidence interval.
#'
#' @importFrom survey withReplicates SE
#' @importFrom WeightedROC WeightedROC WeightedAUC
#' @importFrom stats model.frame model.matrix coef plogis
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
svyAUC <- function(fit, design) {

  # Input Validation
  if (!inherits(design, "svyrep.design")) {
    stop("Error: This function requires a replicate-weights survey design object (created with as.svrepdesign).")
  }
  if (!inherits(fit, "svyglm")) {
    stop("Error: This function is designed for 'svyglm' model objects.")
  }

  outcome_name <- all.vars(fit$formula[[2]])[1]

  auc_statistic <- function(weights, data) {
    model_formula <- formula(fit)
    mf <- model.frame(model_formula, data)
    mm <- model.matrix(model_formula, mf)
    beta <- coef(fit)
    eta <- mm %*% beta
    predictions <- as.vector(plogis(eta))

    outcome <- data[[outcome_name]]
    if(is.factor(outcome)) {
      outcome <- as.numeric(outcome) - 1
    }

    local_data <- data.frame(
      predictions = predictions,
      outcome = outcome,
      w = weights
    )

    local_data <- local_data[local_data$w > 0 & !is.na(local_data$w), ]

    roc_curve <- WeightedROC::WeightedROC(
      guess = local_data$predictions,
      label = local_data$outcome,
      weight = local_data$w
    )
    WeightedROC::WeightedAUC(roc_curve)
  }

  result <- survey::withReplicates(
    design,
    theta = auc_statistic,
    return.replicates = TRUE
  )

  auc_estimate <- result$theta
  se <- survey::SE(result)
  # ci <- stats::confint(result)

  output <- data.frame(
    AUC = auc_estimate,
    SE = se,
    # CI_Lower = ci[1],
    # CI_Upper = ci[2]
    CI_Lower = auc_estimate - 1.96 * se,
    CI_Upper = auc_estimate + 1.96 * se
  )

  rownames(output) <- NULL
  return(output)
}

