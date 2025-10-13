#' Calculate and Optionally Plot a Design-Correct AUC for a Survey Model
#'
#' @description
#' This function calculates the Area Under the Curve (AUC) and its design-correct
#' standard error and 95% confidence interval. It can also generate a plot of
#' the weighted ROC curve.
#'
#' @param fit A fitted model object of class `svyglm`.
#' @param design A replicate-weights survey design object, typically created with `as.svrepdesign`.
#' @param plot A logical value. If `TRUE`, an ROC curve is plotted. Defaults to `FALSE`.
#'
#' @return
#' If `plot = FALSE` (default), returns a `data.frame` with the AUC, SE, and 95% CI.
#' If `plot = TRUE`, invisibly returns a list containing the summary `data.frame`
#' and another `data.frame` with the ROC curve coordinates (TPR and FPR).
#'
#' @importFrom survey withReplicates SE
#' @importFrom WeightedROC WeightedROC WeightedAUC
#' @importFrom stats model.frame model.matrix coef plogis weights
#' @importFrom graphics plot abline title
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
svyAUC <- function(fit, design, plot = FALSE) {

  # Input Validation
  if (!inherits(design, "svyrep.design")) {
    stop("Error: This function requires a replicate-weights survey design object (created with as.svrepdesign).")
  }
  if (!inherits(fit, "svyglm")) {
    stop("Error: This function is designed for 'svyglm' model objects.")
  }

  outcome_name <- all.vars(fit$formula[[2]])[1]

  # Define the statistic function to be used with replicates
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

  # Run the calculation across all replicate weights
  result <- survey::withReplicates(
    design,
    theta = auc_statistic,
    return.replicates = TRUE
  )

  # Manually calculate the confidence interval
  auc_estimate <- result$theta
  se <- survey::SE(result)

  summary_df <- data.frame(
    AUC = auc_estimate,
    SE = se,
    CI_Lower = auc_estimate - 1.96 * se,
    CI_Upper = auc_estimate + 1.96 * se
  )
  rownames(summary_df) <- NULL

  # --- PLOTTING LOGIC ---
  if (plot) {
    # Calculate ROC curve points using the full-sample weights
    full_weights <- weights(design, "sampling")
    roc_data <- auc_statistic(full_weights, design$variables) # Temporarily re-run to get roc_curve

    # Actually need the curve, not just the AUC
    predictions <- predict(fit, newdata = design$variables, type = "response")
    outcome <- design$variables[[outcome_name]]
    if(is.factor(outcome)) {
      outcome <- as.numeric(outcome) - 1
    }

    roc_curve_points <- WeightedROC::WeightedROC(
      guess = predictions,
      label = outcome,
      weight = full_weights
    )

    plot(roc_curve_points$FPR, roc_curve_points$TPR,
         type = 'l',
         xlab = "1 - Specificity (FPR)",
         ylab = "Sensitivity (TPR)",
         main = "Survey-Weighted ROC Curve"
    )
    abline(0, 1, lty = 2)
    title(sub = paste0("AUC = ", round(summary_df$AUC, 3)), adj = 1)

    invisible(list(summary = summary_df, roc_data = roc_curve_points))

  } else {
    return(summary_df)
  }
}

