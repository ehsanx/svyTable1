#' Create a Survey-Weighted Descriptive Statistics Table
#'
#' @description
#' Generates a "Table 1" of descriptive statistics for complex survey data,
#' stratified by an outcome variable. It supports different reporting modes,
#' optional comma formatting, and optional reliability checks based on NCHS
#' Data Presentation Standards.
#'
#' @param design A survey design object created by the survey package.
#' @param strata_var A string with the name of the stratification variable.
#' @param table_vars A character vector of variable names to summarize.
#' @param mode A string specifying the output type: "mixed" (default), "weighted", or "unweighted".
#' @param commas Logical; if TRUE (default), large numbers in counts are formatted with commas.
#' @param reliability_checks Logical; if TRUE, performs reliability checks. Unreliable
#'   estimates are replaced with an asterisk (*). The rules are based on NCHS guidelines.
#'   \itemize{
#'     \item{\strong{For proportions (categorical variables)}, an estimate is suppressed if it meets ANY of the following criteria:
#'       \itemize{
#'         \item{Unweighted Sample Size < 30: The actual number of survey participants in the cell is too small to produce a stable estimate.}
#'         \item{Effective Sample Size < 30: After accounting for the complex survey design, the estimate's precision is equivalent to that from a simple random sample of fewer than 30 people.}
#'         \item{Degrees of Freedom < 8: The variance estimate is itself unstable, making the entire estimate unreliable.}
#'         \item{Confidence Interval Width >= 30\%: The 95\% confidence interval is 30 percentage points wide or wider, indicating extreme imprecision.}
#'         \item{Conditional Relative CI Width > 130\%: If the confidence interval width is between 5\% and 30\%, it is also checked to see if it is excessively large \emph{relative} to the estimate itself.}
#'       }
#'     }
#'     \item{\strong{For means (numeric variables)}, an estimate is suppressed if:
#'       \itemize{
#'         \item{Relative Standard Error (RSE) >= 30\%: The standard error is 30\% or more of the mean's value, indicating high imprecision.}
#'       }
#'     }
#'   }
#'   Defaults to FALSE.
#' @param return_metrics Logical; if TRUE and `reliability_checks` is also TRUE, the function
#'   returns a list containing the formatted table and a detailed data frame of the
#'   reliability metrics. This includes the RSE and specific TRUE/FALSE columns
#'   (e.g., `fail_n_30`, `fail_eff_n_30`) for each suppression rule. Defaults to FALSE.
#'
#' @return If `return_metrics` is FALSE (default), returns a data.frame. If TRUE,
#'   returns a list with two elements: `formatted_table` and `reliability_metrics`.
#'
#' @importFrom survey svytable svymean svyby svyvar svyciprop degf SE
#' @import stats
#'
#' @export
#'
#' @examples
#' # Ensure required packages are loaded
#' if (requireNamespace("survey", quietly = TRUE) &&
#'     requireNamespace("NHANES", quietly = TRUE) &&
#'     requireNamespace("dplyr", quietly = TRUE)) {
#'
#'   # 1. Prepare Data using NHANES
#'   data(NHANESraw, package = "NHANES")
#'   nhanes_adults_with_na <- NHANESraw %>%
#'     dplyr::filter(Age >= 20) %>%
#'     dplyr::mutate(
#'       ObeseStatus = factor(ifelse(BMI >= 30, "Obese", "Not Obese"),
#'                            levels = c("Not Obese", "Obese"))
#'     )
#'
#'   adult_design_with_na <- survey::svydesign(
#'     id = ~SDMVPSU,
#'     strata = ~SDMVSTRA,
#'     weights = ~WTMEC2YR,
#'     nest = TRUE,
#'     data = nhanes_adults_with_na
#'   )
#'
#'   # 2. Basic Example: Create a simple Table 1
#'   vars_to_summarize <- c("Age", "Gender", "Race1", "Education")
#'   table1 <- svytable1(
#'     design = adult_design_with_na,
#'     strata_var = "ObeseStatus",
#'     table_vars = vars_to_summarize
#'   )
#'   print(table1)
#'
#'   # 3. Advanced Example: Use reliability checks and get detailed metrics
#'   results <- svytable1(
#'     design = adult_design_with_na,
#'     strata_var = "ObeseStatus",
#'     table_vars = vars_to_summarize,
#'     reliability_checks = TRUE,
#'     return_metrics = TRUE
#'   )
#'
#'   # View the table with unreliable estimates suppressed (*)
#'   print(results$formatted_table)
#'
#'   # View the detailed report card of reliability checks
#'   print(results$reliability_metrics)
#' }
svytable1 <- function(design, strata_var, table_vars,
                      mode = "mixed", commas = TRUE,
                      reliability_checks = FALSE, return_metrics = FALSE) {

  # --- 1. Input Validation ---
  df <- design$variables
  all_vars <- c(strata_var, table_vars)
  missing_vars <- all_vars[!all_vars %in% names(df)]
  if (length(missing_vars) > 0) {
    stop(paste("The following variables were not found in the data:",
               paste(missing_vars, collapse = ", ")))
  }
  w_counts_strata <- NULL

  # --- Helper function for formatting ---
  format_num <- function(n, is_weighted) {
    if (is_weighted) n <- round(n)
    if (commas) return(format(n, big.mark = ","))
    return(as.character(n))
  }

  # --- 2. Table Generation ---
  if (nrow(df) == 0) return(data.frame(Error = "Input data has 0 rows"))

  if (any(is.na(df[[strata_var]]))) {
    strata_as_char <- as.character(df[[strata_var]])
    strata_as_char[is.na(strata_as_char)] <- "Missing"
    df[[strata_var]] <- as.factor(strata_as_char)
    design$variables <- df
  }

  df[[strata_var]] <- droplevels(df[[strata_var]])
  strata_levels <- levels(df[[strata_var]])
  unweighted_n_overall <- nrow(df)
  unweighted_n_strata <- table(df[[strata_var]])

  header_row <- data.frame(Variable = "n", Level = "")
  if (mode == "mixed") {
    header_row$Overall <- format_num(unweighted_n_overall, FALSE)
    for (lvl in strata_levels)
      header_row[[lvl]] <- format_num(unweighted_n_strata[lvl], FALSE)
  } else if (mode == "weighted") {
    weighted_n_strata <- svytable(as.formula(paste0("~", strata_var)), design)
    header_row$Overall <- format_num(sum(weights(design, "sampling"), na.rm=TRUE), TRUE)
    for (lvl in strata_levels)
      header_row[[lvl]] <- format_num(weighted_n_strata[lvl], TRUE)
  } else { # unweighted
    header_row$Overall <- format_num(unweighted_n_overall, FALSE)
    for (lvl in strata_levels)
      header_row[[lvl]] <- format_num(unweighted_n_strata[lvl], FALSE)
  }

  results_list <- list(header_row)

  if (reliability_checks && return_metrics) {
    metrics_list <- list()
  }

  for (current_var_name in table_vars) {
    var_formula <- as.formula(paste0("~", current_var_name))
    strata_formula <- as.formula(paste0("~", strata_var))

    var_header_row <- as.data.frame(setNames(as.list(c(current_var_name, rep("", ncol(header_row) - 1))), names(header_row)), check.names = FALSE)
    results_list[[length(results_list) + 1]] <- var_header_row

    current_var_vector <- design$variables[[current_var_name]]

    if (is.factor(current_var_vector)) {
      # --- Code for factor variables ---
      if (any(is.na(current_var_vector))) {
        original_levels <- levels(current_var_vector)
        var_as_char <- as.character(current_var_vector)
        var_as_char[is.na(var_as_char)] <- "Missing"
        design$variables[[current_var_name]] <- factor(
          var_as_char,
          levels = c(original_levels, "Missing")
        )
      }

      if (reliability_checks) {
        reliability_list <- list()

        for (s_lvl in strata_levels) {
          sub_design <- subset(design, get(strata_var) == s_lvl)
          if(nrow(sub_design) == 0) next

          prop_res <- try(svymean(var_formula, sub_design, na.rm = FALSE, deff = TRUE), silent = TRUE)
          if (inherits(prop_res, "try-error")) next

          var_levels <- levels(sub_design$variables[[current_var_name]])
          ci_low_vals <- c(); ci_high_vals <- c()
          for(v_lvl in var_levels){
            prop_formula <- as.formula(paste0("~I(`", current_var_name, "` == '", v_lvl, "')"))
            ci_level <- svyciprop(prop_formula, sub_design, method = "beta", na.rm = TRUE)
            ci_low_vals <- c(ci_low_vals, attr(ci_level, "ci")[1])
            ci_high_vals <- c(ci_high_vals, attr(ci_level, "ci")[2])
          }

          reliability_list[[s_lvl]] <- list(
            prop = as.numeric(prop_res), se = as.numeric(SE(prop_res)),
            n = as.numeric(table(sub_design$variables[[current_var_name]])),
            deff = diag(attr(prop_res, "deff")), ci_low = ci_low_vals,
            ci_high = ci_high_vals, df = degf(sub_design)
          )
        }
      }

      uw_counts_overall <- table(design$variables[[current_var_name]])
      uw_counts_strata <- table(design$variables[[current_var_name]], design$variables[[strata_var]])
      w_counts_overall <- svytable(var_formula, design, round = TRUE)

      for (lvl in levels(design$variables[[current_var_name]])) {
        row_data <- data.frame(Variable = "", Level = lvl, stringsAsFactors = FALSE)
        col_name_pct <- paste0(current_var_name, lvl)

        w_pcts_overall <- svymean(var_formula, design, na.rm = FALSE)
        if (mode == "mixed") {
          row_data$Overall <- sprintf("%s (%.1f%%)", format_num(uw_counts_overall[lvl], FALSE), w_pcts_overall[col_name_pct] * 100)
        } else if (mode == "weighted") {
          row_data$Overall <- sprintf("%s (%.1f%%)", format_num(w_counts_overall[lvl], TRUE), w_pcts_overall[col_name_pct] * 100)
        } else {
          unweighted_pct <- 100 * uw_counts_overall[lvl] / sum(uw_counts_overall)
          row_data$Overall <- sprintf("%s (%.1f%%)", format_num(uw_counts_overall[lvl], FALSE), unweighted_pct)
        }

        for (s_lvl in strata_levels) {
          cell_value <- ""; pct_val <- NA
          if (reliability_checks && !is.null(reliability_list[[s_lvl]])) {
            level_index <- which(levels(design$variables[[current_var_name]]) == lvl)

            if(level_index > length(reliability_list[[s_lvl]]$n)) {
              uw_count <- uw_counts_strata[lvl, s_lvl]
              cell_value <- if (uw_count > 0) sprintf("%s (0.0%%)", format_num(uw_count, FALSE)) else "0 (0.0%)"
            } else {
              metrics <- reliability_list[[s_lvl]]; n <- metrics$n[level_index]
              deff <- metrics$deff[level_index]; df <- metrics$df
              ci_low <- metrics$ci_low[level_index]; ci_high <- metrics$ci_high[level_index]
              pct_val <- metrics$prop[level_index]; se <- metrics$se[level_index]

              effective_n <- if(!is.na(deff) && deff > 0) n / deff else 0
              ciw <- ci_high - ci_low
              rciw <- if(!is.na(pct_val) && pct_val > 0) (ciw / pct_val) * 100 else Inf
              rse <- if(!is.na(pct_val) && pct_val > 0) (se / pct_val) * 100 else Inf

              fail_n_30 <- is.na(n) || n < 30
              fail_eff_n_30 <- !is.na(effective_n) && effective_n < 30
              fail_df_8 <- !is.na(df) && df < 8
              fail_ciw_30 <- !is.na(ciw) && ciw >= 0.30
              fail_rciw_130 <- !is.na(ciw) && ciw > 0.05 && !is.na(rciw) && rciw > 130
              fail_rse_30 <- !is.na(rse) && rse >= 30

              suppress <- fail_n_30 || fail_eff_n_30 || fail_df_8 || fail_ciw_30 || fail_rciw_130
              if (suppress) cell_value <- "*"

              if (return_metrics) {
                metrics_list[[length(metrics_list) + 1]] <- data.frame(
                  stratum = s_lvl, variable = current_var_name, level = lvl,
                  n = n, df = df, deff = deff, effective_n = effective_n,
                  ci_low = ci_low, ci_high = ci_high, rse = rse,
                  suppressed = suppress, fail_n_30 = fail_n_30, fail_eff_n_30 = fail_eff_n_30,
                  fail_df_8 = fail_df_8, fail_ciw_30 = fail_ciw_30,
                  fail_rciw_130 = fail_rciw_130, fail_rse_30 = fail_rse_30
                )
              }
            }
          }

          if (cell_value == "") {
            if (is.na(pct_val)) {
              w_pcts_strata_lazy <- svyby(var_formula, strata_formula, design, svymean, na.rm = FALSE)
              pct_val <- w_pcts_strata_lazy[w_pcts_strata_lazy[, 1] == s_lvl, col_name_pct]
              if(length(pct_val) == 0 || is.na(pct_val)) pct_val <- 0
            }
            if (mode == "mixed") {
              uw_count <- uw_counts_strata[lvl, s_lvl]
              cell_value <- sprintf("%s (%.1f%%)", format_num(uw_count, FALSE), pct_val * 100)
            } else if (mode == "weighted") {
              w_count <- w_counts_strata[lvl, s_lvl]
              cell_value <- sprintf("%s (%.1f%%)", format_num(w_count, TRUE), pct_val * 100)
            } else {
              uw_count <- uw_counts_strata[lvl, s_lvl]
              unweighted_pct_strata <- 100 * uw_count / sum(uw_counts_strata[, s_lvl])
              if (is.nan(unweighted_pct_strata)) unweighted_pct_strata <- 0
              cell_value <- sprintf("%s (%.1f%%)", format_num(uw_count, FALSE), unweighted_pct_strata)
            }
          }
          row_data[[s_lvl]] <- cell_value
        }
        results_list[[length(results_list) + 1]] <- row_data
      }
    } else if (is.numeric(current_var_vector)) {
      # --- Code for numeric variables ---
      row_data <- data.frame(Variable = "", Level = "Mean (SD)", stringsAsFactors = FALSE)

      if (mode %in% c("mixed", "weighted")) {
        mean_overall_w <- svymean(var_formula, design, na.rm = TRUE)
        var_overall_w <- svyvar(var_formula, design, na.rm = TRUE)

        suppress_overall <- FALSE
        if (reliability_checks) {
          se_overall <- SE(mean_overall_w)[1]
          mean_val_overall <- mean_overall_w[1]
          rse_overall <- if (!is.na(mean_val_overall) && mean_val_overall != 0) (se_overall / abs(mean_val_overall)) * 100 else Inf
          fail_rse_30_overall <- !is.na(rse_overall) && rse_overall >= 30
          suppress_overall <- fail_rse_30_overall

          if (return_metrics) {
            metrics_list[[length(metrics_list) + 1]] <- data.frame(
              stratum = "Overall", variable = current_var_name, level = "Mean (SD)",
              n = NA, df = NA, deff = NA, effective_n = NA, ci_low = NA, ci_high = NA, rse = rse_overall,
              suppressed = suppress_overall, fail_n_30 = NA, fail_eff_n_30 = NA, fail_df_8 = NA,
              fail_ciw_30 = NA, fail_rciw_130 = NA, fail_rse_30 = fail_rse_30_overall)
          }
        }
        row_data$Overall <- if(suppress_overall) "*" else sprintf("%.2f (%.2f)", mean_overall_w[1], sqrt(var_overall_w[1]))

        means_by_strata_w <- svyby(var_formula, strata_formula, design, svymean, na.rm = TRUE)
        vars_by_strata_w <- svyby(var_formula, strata_formula, design, svyvar, na.rm = TRUE)

        for(i in 1:nrow(means_by_strata_w)){
          s_lvl <- as.character(means_by_strata_w[i, 1])
          mean_val <- means_by_strata_w[i, 2]
          sd_val <- sqrt(vars_by_strata_w[i, 2])

          suppress_strata <- FALSE
          if (reliability_checks) {
            se_val <- SE(means_by_strata_w)[i]
            rse <- if (!is.na(mean_val) && mean_val != 0) (se_val / abs(mean_val)) * 100 else Inf
            fail_rse_30 <- !is.na(rse) && rse >= 30
            suppress_strata <- fail_rse_30

            if (return_metrics) {
              metrics_list[[length(metrics_list) + 1]] <- data.frame(
                stratum = s_lvl, variable = current_var_name, level = "Mean (SD)",
                n = NA, df = NA, deff = NA, effective_n = NA, ci_low = NA, ci_high = NA, rse = rse,
                suppressed = suppress_strata, fail_n_30 = NA, fail_eff_n_30 = NA, fail_df_8 = NA,
                fail_ciw_30 = NA, fail_rciw_130 = NA, fail_rse_30 = fail_rse_30)
            }
          }
          row_data[[s_lvl]] <- if(suppress_strata) "*" else sprintf("%.2f (%.2f)", mean_val, sd_val)
        }

      } else {
        unweighted_mean_overall <- mean(current_var_vector, na.rm = TRUE)
        unweighted_sd_overall <- sd(current_var_vector, na.rm = TRUE)
        row_data$Overall <- sprintf("%.2f (%.2f)", unweighted_mean_overall, unweighted_sd_overall)

        unweighted_mean_strata <- tapply(current_var_vector, design$variables[[strata_var]], mean, na.rm = TRUE)
        unweighted_sd_strata <- tapply(current_var_vector, design$variables[[strata_var]], sd, na.rm = TRUE)

        for(s_lvl in strata_levels){
          row_data[[s_lvl]] <- sprintf("%.2f (%.2f)", unweighted_mean_strata[s_lvl], unweighted_sd_strata[s_lvl])
        }
      }
      results_list[[length(results_list) + 1]] <- row_data

      if (any(is.na(current_var_vector))) {
        missing_row <- data.frame(Variable = "", Level = "Missing, n (%)", stringsAsFactors = FALSE)
        design_temp <- update(design, is_missing = as.numeric(is.na(current_var_vector)))

        uw_n_missing <- sum(is.na(current_var_vector))
        w_pct_missing <- svymean(~is_missing, design_temp, na.rm = TRUE)
        missing_row$Overall <- sprintf("%s (%.1f%%)", format_num(uw_n_missing, FALSE), w_pct_missing * 100)

        missing_by_strata <- svyby(~is_missing, strata_formula, design_temp, svymean, na.rm = TRUE)

        for (i in 1:nrow(missing_by_strata)) {
          s_lvl <- as.character(missing_by_strata[i, 1])
          uw_n_strata_missing <- sum(is.na(current_var_vector[design$variables[[strata_var]] == s_lvl]))
          missing_row[[s_lvl]] <- sprintf("%s (%.1f%%)", format_num(uw_n_strata_missing, FALSE), missing_by_strata[i, 2] * 100)
        }
        results_list[[length(results_list) + 1]] <- missing_row
      }
    }
  }

  final_table <- do.call(rbind, results_list)
  rownames(final_table) <- NULL

  if (reliability_checks && return_metrics) {
    if (length(metrics_list) > 0) {
      metrics_df <- do.call(rbind, metrics_list)

      # --- NEW: Round numeric columns to 2 decimal places ---
      cols_to_format <- sapply(metrics_df, is.numeric)
      metrics_df[cols_to_format] <- lapply(metrics_df[cols_to_format], function(x) round(x, 2))

    } else {
      metrics_df <- data.frame()
    }
    return(list(formatted_table = final_table, reliability_metrics = metrics_df))
  } else {
    return(final_table)
  }
}
