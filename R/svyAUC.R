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
#' If `plot = FALSE` (default), a one-row `data.frame` with columns `AUC`, `SE`,
#' `CI_Lower` and `CI_Upper`. The standard error is computed across the design's
#' replicate weights and the 95\% confidence interval is built on the logit scale
#' (so it stays within `[0, 1]`); both are therefore conditional on the
#' replication scheme used to build `design`. If `plot = TRUE`, an ROC curve is
#' drawn and a list is returned invisibly with the summary `data.frame` and a
#' `data.frame` of ROC coordinates (TPR and FPR).
#'
#' @importFrom survey withReplicates SE degf
#' @importFrom WeightedROC WeightedROC WeightedAUC
#' @importFrom stats model.frame model.matrix coef plogis qlogis qt weights
#' @importFrom graphics plot abline title
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(nhanes_mortality, package = "svyTable1")
#' nhanes_mortality$htn01 <- as.numeric(nhanes_mortality$htn == "Yes")
#'
#' # svyAUC requires a replicate-weights design.
#' design <- survey::svydesign(
#'   id = ~psu, strata = ~strata, weights = ~survey_weight,
#'   nest = TRUE, data = nhanes_mortality
#' )
#' rep_design <- survey::as.svrepdesign(design)
#'
#' fit <- survey::svyglm(htn01 ~ age + sex + smoking,
#'                       design = rep_design, family = quasibinomial())
#'
#' # AUC with a design-correct SE and a logit-scale 95% CI.
#' svyAUC(fit, rep_design)
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

  # Point estimate and design-correct SE (variance across replicate weights).
  auc_estimate <- as.numeric(result$theta)
  se <- as.numeric(survey::SE(result))

  # Critical value from the design degrees of freedom rather than a fixed 1.96.
  df <- survey::degf(design)
  crit <- stats::qt(0.975, df)

  # Build the 95% CI on the logit scale and back-transform, so the interval
  # cannot fall outside the valid [0, 1] range of an AUC. At the boundaries
  # (where the logit is undefined) fall back to a clamped Wald interval.
  if (is.finite(auc_estimate) && auc_estimate > 0 && auc_estimate < 1) {
    se_logit <- se / (auc_estimate * (1 - auc_estimate))
    ci_lower <- stats::plogis(stats::qlogis(auc_estimate) - crit * se_logit)
    ci_upper <- stats::plogis(stats::qlogis(auc_estimate) + crit * se_logit)
  } else {
    ci_lower <- max(0, auc_estimate - crit * se)
    ci_upper <- min(1, auc_estimate + crit * se)
  }

  summary_df <- data.frame(
    AUC = auc_estimate,
    SE = se,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper
  )
  rownames(summary_df) <- NULL

  # --- PLOTTING LOGIC ---
  if (plot) {
    # Calculate ROC curve points using the full-sample weights.
    full_weights <- weights(design, "sampling")
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

