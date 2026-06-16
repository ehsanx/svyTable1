#' Calculate Simple (Stratum-Specific) Effects from a Joint Variable Model
#'
#' @description
#' Replicates the output of an interaction-term model (e.g., simple effects
#' from `publish()` or `emmeans`) by calculating all stratum-specific
#' comparisons from a joint-variable model. Allows output on log or ratio scale
#' with consistent column structure, including scale-appropriate standard errors.
#'
#' @param joint_model A fitted model object (e.g., `svycoxph`, `svyglm`) that
#'   used a combined joint variable (e.g., `~ A_B`).
#' @param joint_var_name Character string. The name of the joint variable as
#'   used in the model formula (e.g., `"Race1_ObeseStatus"`).
#' @param factor1_name Character string. The name of the *first* logical factor
#'   used to create the joint variable (e.g., `"Race1"`). Used for naming output rows.
#' @param factor2_name Character string. The name of the *second* logical factor
#'   used to create the joint variable (e.g., `"ObeseStatus"`). Used for naming output rows.
#' @param factor1_levels Character vector. All levels of the *first* logical factor,
#'   with the reference level *first*.
#' @param factor2_levels Character vector. All levels of the *second* logical factor,
#'   with the reference level *first*.
#' @param level_separator Character string. The separator used to create the
#'   joint level names (e.g., `"_"` or `"."`).
#'   Default is `"."` (the R default for `interaction()`).
#' @param scale Character string. The scale for the output estimates, CIs, and SE.
#'   Options are `"ratio"` (default, e.g., OR/HR) or `"log"` (log-OR/log-HR).
#' @param digits Integer. Number of decimal places for rounding estimates, SE,
#'   and CIs when `scale = "ratio"`. Default is 2. (Set to NULL for no rounding).
#' @param conf.level Confidence level for the interval (default 0.95).
#'
#' @return
#' A `tibble` (data frame) with columns:
#' \itemize{
#'   \item `Comparison`: A description of the simple effect, styled similarly
#'     to the `Publish` package (e.g., "ObeseStatus(Obese vs Not Obese): Race1(White)").
#'   \item `Estimate`: The point estimate on the specified `scale`.
#'   \item `SE`: The standard error on the specified `scale`. Calculated using the
#'     delta method for `scale = "ratio"`.
#'   \item `CI.low`: The lower bound of the confidence interval (on the specified `scale`).
#'   \item `CI.upp`: The upper bound of the confidence interval (on the specified `scale`).
#'   \item `p_value`: The p-value for the simple effect (tests H0: logEstimate=0 or Estimate=1).
#' }
#'
#' @importFrom stats coef vcov qnorm pnorm na.omit
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr %>% mutate bind_rows select case_when rename relocate
#' @importFrom rlang := .data
#' @importFrom msm deltamethod
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(nhanes_mortality, package = "svyTable1")
#' nhanes_mortality$htn01 <- as.numeric(nhanes_mortality$htn == "Yes")
#'
#' # Build a joint variable from two factors, then fit a joint-variable model.
#' nhanes_mortality$sex_insulin <- interaction(
#'   nhanes_mortality$sex, nhanes_mortality$insulin, sep = "_"
#' )
#'
#' design <- survey::svydesign(
#'   id = ~psu, strata = ~strata, weights = ~survey_weight,
#'   nest = TRUE, data = nhanes_mortality
#' )
#'
#' joint_fit <- survey::svyglm(htn01 ~ sex_insulin + age,
#'                             design = design, family = quasibinomial())
#'
#' # Stratum-specific (simple) effects reconstructed from the joint model.
#' inteffects(
#'   joint_model     = joint_fit,
#'   joint_var_name  = "sex_insulin",
#'   factor1_name    = "sex",
#'   factor2_name    = "insulin",
#'   factor1_levels  = c("Male", "Female"),  # reference level first
#'   factor2_levels  = c("No", "Yes"),       # reference level first
#'   level_separator = "_",
#'   scale = "ratio"
#' )
#' }
inteffects <- function(joint_model,
                       joint_var_name,
                       factor1_name, # Added for naming
                       factor2_name, # Added for naming
                       factor1_levels,
                       factor2_levels,
                       level_separator = ".",
                       scale = c("ratio", "log"),
                       digits = 2,
                       conf.level = 0.95) {

  scale <- match.arg(scale)

  if (!requireNamespace("msm", quietly = TRUE)) {
    stop("Package 'msm' needed for this function to work. Please install it.", call. = FALSE)
  }


  # --- 1. Extract Model Components ---
  beta <- tryCatch(stats::coef(joint_model), error = function(e) NULL)
  V <- tryCatch(stats::vcov(joint_model), error = function(e) NULL)

  if (is.null(beta) || is.null(V)) {
    warning("Could not extract coefficients or vcov from the model.")
    return(NULL)
  }
  all_coef_names <- names(beta)

  # --- 2. Build Full Padded beta and vcov matrices ---
  all_joint_levels <- expand.grid(f1 = factor1_levels, f2 = factor2_levels, stringsAsFactors = FALSE)
  joint_level_names <- paste(all_joint_levels$f1, all_joint_levels$f2, sep = level_separator)
  n_levels <- length(joint_level_names)

  ref_level1 <- factor1_levels[1]
  ref_level2 <- factor2_levels[1]
  ref_joint_name <- paste(ref_level1, ref_level2, sep = level_separator)

  coef_map <- list()
  beta_map <- list()
  beta_map[[ref_joint_name]] <- 0.0

  for (name in joint_level_names) {
    if (name == ref_joint_name) next
    model_coef_name <- paste0(joint_var_name, name)
    if (model_coef_name %in% all_coef_names) {
      coef_map[[name]] <- model_coef_name
      beta_map[[name]] <- beta[model_coef_name]
    } else {
      warning(paste("Could not find coefficient:", model_coef_name, ". Skipping comparisons involving it."))
      beta_map[[name]] <- NA
    }
  }

  V_full <- matrix(0, nrow = n_levels, ncol = n_levels,
                   dimnames = list(joint_level_names, joint_level_names))
  non_ref_joint_names <- names(coef_map)
  model_coef_names_in_map <- unlist(coef_map)

  if (any(is.na(model_coef_names_in_map))) {
    warning("Some coefficients were missing, variance matrix will be incomplete.")
    model_coef_names_in_map <- stats::na.omit(model_coef_names_in_map)
    non_ref_joint_names <- names(which(!is.na(coef_map)))
  }

  if (length(model_coef_names_in_map) > 0) {
    if (all(model_coef_names_in_map %in% rownames(V)) && all(model_coef_names_in_map %in% colnames(V))) {
      V_sub <- V[model_coef_names_in_map, model_coef_names_in_map, drop = FALSE]
      if (all(dim(V_sub) == c(length(non_ref_joint_names), length(non_ref_joint_names)))) {
        V_full[non_ref_joint_names, non_ref_joint_names] <- V_sub
      } else {
        warning("Dimension mismatch when building padded VCV. Check for duplicate level names or issues with VCV matrix.")
      }
    } else {
      warning("Could not subset VCV matrix; some coefficient names might be missing from it.")
    }
  }


  results_list <- list()

  # --- 3. Helper function to calculate difference ---
  calculate_difference <- function(group1_name, group2_name, comparison_desc) {
    loghr1 <- beta_map[[group1_name]]
    loghr2 <- beta_map[[group2_name]]
    if (is.na(loghr1) || is.na(loghr2)) return(NULL)
    diff_loghr <- loghr1 - loghr2 # This is the logEstimate

    # Variance of the difference on the log scale
    var_A <- V_full[group1_name, group1_name]
    var_B <- V_full[group2_name, group2_name]
    cov_AB <- V_full[group1_name, group2_name]
    var_diff_log <- var_A + var_B - 2 * cov_AB

    if (var_diff_log < 0 && abs(var_diff_log) < 1e-10) var_diff_log <- 0

    if (var_diff_log < 0 || is.na(var_diff_log)) {
      warning(paste("Negative or NA variance for log comparison:", comparison_desc))
      se_diff_log <- NA
    } else {
      se_diff_log <- sqrt(var_diff_log)
    }

    # Calculate SE on ratio scale using delta method: SE(exp(X)) approx exp(X) * SE(X)
    # More accurately calculated using msm::deltamethod(~exp(x1), mean, variance)
    se_diff_ratio <- NA
    if (!is.na(diff_loghr) && !is.na(var_diff_log) && var_diff_log >= 0) {
      se_diff_ratio <- tryCatch({
        msm::deltamethod(~exp(x1), diff_loghr, var_diff_log)
      }, error = function(e) {
        warning("Delta method failed for ratio SE in comparison: ", comparison_desc, " Error: ", e$message)
        NA # Assign NA if deltamethod fails
      })
    }


    return(list(Comparison = comparison_desc,
                logEstimate = diff_loghr,
                SE_log = se_diff_log,
                SE_ratio = se_diff_ratio))
  }

  # --- 4. Generate Simple Effects of Factor 2, stratified by Factor 1 ---
  for (lvl1 in factor1_levels) {
    for (lvl2 in factor2_levels[-1]) {
      comp_desc <- sprintf("%s(%s vs %s): %s(%s)",
                           factor2_name, lvl2, ref_level2,
                           factor1_name, lvl1)
      g1_name <- paste(lvl1, lvl2, sep = level_separator)
      g2_name <- paste(lvl1, ref_level2, sep = level_separator)
      res <- calculate_difference(g1_name, g2_name, comp_desc)
      if (!is.null(res)) results_list[[length(results_list) + 1]] <- res
    }
  }

  # --- 5. Generate Simple Effects of Factor 1, stratified by Factor 2 ---
  for (lvl2 in factor2_levels) {
    for (lvl1 in factor1_levels[-1]) {
      comp_desc <- sprintf("%s(%s vs %s): %s(%s)",
                           factor1_name, lvl1, ref_level1,
                           factor2_name, lvl2)
      g1_name <- paste(lvl1, lvl2, sep = level_separator)
      g2_name <- paste(ref_level1, lvl2, sep = level_separator)
      res <- calculate_difference(g1_name, g2_name, comp_desc)
      if (!is.null(res)) results_list[[length(results_list) + 1]] <- res
    }
  }

  # --- 6. Format Output Table ---
  if (length(results_list) == 0) {
    warning("No simple effects could be calculated.")
    return(tibble::tibble())
  }

  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  output_df <- dplyr::bind_rows(lapply(results_list, tibble::as_tibble))

  # Calculate base values needed for both scales
  output_df <- output_df %>%
    # Use .data pronoun for all columns
    dplyr::mutate(
      p_value = dplyr::case_when(
        is.na(.data$SE_log) ~ NA_real_,
        .data$SE_log == 0 & .data$logEstimate == 0 ~ 1.0,
        .data$SE_log == 0 & .data$logEstimate != 0 ~ 0.0,
        TRUE ~ 2 * stats::pnorm(-abs(.data$logEstimate / .data$SE_log))
      ),
      Estimate_ratio = exp(.data$logEstimate),
      CI.low_ratio = dplyr::case_when(
        !is.na(.data$SE_log) ~ exp(.data$logEstimate - z * .data$SE_log),
        TRUE ~ NA_real_
      ),
      CI.upp_ratio = dplyr::case_when(
        !is.na(.data$SE_log) ~ exp(.data$logEstimate + z * .data$SE_log),
        TRUE ~ NA_real_
      ),
      CI.low_log = dplyr::case_when(
        !is.na(.data$SE_log) ~ .data$logEstimate - z * .data$SE_log,
        TRUE ~ NA_real_
      ),
      CI.upp_log = dplyr::case_when(
        !is.na(.data$SE_log) ~ .data$logEstimate + z * .data$SE_log,
        TRUE ~ NA_real_
      )
    )

  # Select and format columns based on requested scale
  if (scale == "log") {
    output_df <- output_df %>%
      # Use .data pronoun
      dplyr::select(
        "Comparison",
        Estimate = "logEstimate",
        SE = "SE_log",
        CI.low = "CI.low_log",
        CI.upp = "CI.upp_log",
        "p_value"
      )

  } else { # scale == "ratio"
    # Rounding logic, only if digits is not NULL
    if (!is.null(digits)) {
      output_df <- output_df %>%
        # Use .data pronoun
        dplyr::mutate(
          Estimate_ratio = round(.data$Estimate_ratio, digits),
          SE_ratio = round(.data$SE_ratio, digits), # Round ratio SE too
          CI.low_ratio = round(.data$CI.low_ratio, digits),
          CI.upp_ratio = round(.data$CI.upp_ratio, digits)
        )
    }

    output_df <- output_df %>%
      # Use .data pronoun
      dplyr::select(
        "Comparison",
        Estimate = "Estimate_ratio",
        SE = "SE_ratio", # Use ratio SE
        CI.low = "CI.low_ratio",
        CI.upp = "CI.upp_ratio",
        "p_value"
      )
  }

  # Final rename to match requested column names exactly
  output_df <- output_df %>%
    # Use .data pronoun
    dplyr::rename(
      `p-value` = "p_value"
    ) %>%
    dplyr::relocate("Comparison", "Estimate", "SE", "CI.low", "CI.upp", "p-value")


  return(output_df)
}
