#' @title Create a Survey-Weighted Descriptive Statistics Table
#'
#' @description
#' Generates a "Table 1" of descriptive statistics for complex survey data,
#' stratified by an outcome variable. It supports different reporting modes and
#' optional comma formatting for large numbers. For subpopulation analysis, it is
#' recommended to use `subset()` on the survey design object before passing it
#' to this function.
#'
#' @param design A survey design object created by the `survey` package.
#' @param strata_var A string with the name of the stratification variable.
#' @param table_vars A character vector of variable names to summarize.
#' @param mode A string specifying the output type:
#'   "mixed" (default): Unweighted n and weighted % or mean(SD).
#'   "weighted": Weighted n/mean and weighted %/SD.
#'   "unweighted": Unweighted n/mean and unweighted %/SD.
#' @param commas Logical; if TRUE (default), large numbers in counts are
#'   formatted with commas for readability.
#'
#' @return A data.frame formatted as a descriptive statistics table.
#'
#' @importFrom survey svytable svymean svyby svyvar
#' @import stats
#'
#' @export
svytable1 <- function(design, strata_var, table_vars,
                      mode = "mixed", commas = TRUE) {

  # --- 1. Input Validation ---
  df <- design$variables
  all_vars <- c(strata_var, table_vars)
  missing_vars <- all_vars[!all_vars %in% names(df)]
  if (length(missing_vars) > 0) {
    stop(paste("The following variables were not found in the data:",
               paste(missing_vars, collapse = ", ")))
  }

  # --- Helper function for formatting ---
  format_num <- function(n, is_weighted) {
    if (is_weighted) n <- round(n)
    if (commas) return(format(n, big.mark = ","))
    return(as.character(n))
  }

  # --- 2. Table Generation ---
  if (nrow(df) == 0) return(data.frame(Error = "Input data has 0 rows"))
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
    header_row$Overall <- format_num(sum(weights(design)), TRUE)
    for (lvl in strata_levels)
      header_row[[lvl]] <- format_num(weighted_n_strata[lvl], TRUE)
  } else { # unweighted
    header_row$Overall <- format_num(unweighted_n_overall, FALSE)
    for (lvl in strata_levels)
      header_row[[lvl]] <- format_num(unweighted_n_strata[lvl], FALSE)
  }

  results_list <- list(header_row)

  for (var in table_vars) {
    var_formula <- as.formula(paste0("~", var))
    strata_formula <- as.formula(paste0("~", strata_var))
    var_header_row <- as.data.frame(setNames(as.list(c(var, rep("", ncol(header_row) - 1))), names(header_row)))
    results_list[[length(results_list) + 1]] <- var_header_row

    if (is.factor(df[[var]])) {
      df[[var]] <- droplevels(df[[var]])
      unweighted_counts_overall <- table(df[[var]])
      unweighted_pcts_overall <- prop.table(unweighted_counts_overall) * 100
      unweighted_counts_strata <- table(df[[var]], df[[strata_var]])
      unweighted_pcts_strata <- prop.table(unweighted_counts_strata, margin = 2) * 100

      if (mode %in% c("weighted", "mixed")) {
        weighted_counts_overall <- svytable(var_formula, design)
        weighted_pcts_overall <- svymean(var_formula, design, na.rm = TRUE) * 100
        weighted_counts_strata <- svytable(as.formula(paste0("~", var, "+", strata_var)), design)
        weighted_pcts_strata <- svyby(var_formula, strata_formula, design, svymean, na.rm = TRUE)
      }

      for (lvl in levels(df[[var]])) {
        row_data <- data.frame(Variable = "", Level = lvl, stringsAsFactors = FALSE)
        if (mode == "mixed") {
          val <- sprintf("%s (%.1f%%)", format_num(unweighted_counts_overall[lvl], FALSE),
                         weighted_pcts_overall[paste0(var, lvl)])
        } else if (mode == "weighted") {
          val <- sprintf("%s (%.1f%%)", format_num(weighted_counts_overall[lvl], TRUE),
                         weighted_pcts_overall[paste0(var, lvl)])
        } else {
          val <- sprintf("%s (%.1f%%)", format_num(unweighted_counts_overall[lvl], FALSE),
                         unweighted_pcts_overall[lvl])
        }
        row_data$Overall <- val

        for (s_lvl in strata_levels) {
          if (mode == "mixed") {
            val <- sprintf("%s (%.1f%%)", format_num(unweighted_counts_strata[lvl, s_lvl], FALSE),
                           weighted_pcts_strata[s_lvl, paste0(var, lvl)] * 100)
          } else if (mode == "weighted") {
            val <- sprintf("%s (%.1f%%)", format_num(weighted_counts_strata[lvl, s_lvl], TRUE),
                           weighted_pcts_strata[s_lvl, paste0(var, lvl)] * 100)
          } else {
            val <- sprintf("%s (%.1f%%)", format_num(unweighted_counts_strata[lvl, s_lvl], FALSE),
                           unweighted_pcts_strata[lvl, s_lvl])
          }
          row_data[[s_lvl]] <- val
        }
        results_list[[length(results_list) + 1]] <- row_data
      }
    } else if (is.numeric(df[[var]])) {
      unweighted_mean_overall <- mean(df[[var]], na.rm = TRUE)
      unweighted_sd_overall <- sd(df[[var]], na.rm = TRUE)
      unweighted_mean_strata <- tapply(df[[var]], df[[strata_var]], mean, na.rm = TRUE)
      unweighted_sd_strata <- tapply(df[[var]], df[[strata_var]], sd, na.rm = TRUE)
      if (mode %in% c("weighted", "mixed")) {
        weighted_mean_overall <- svymean(var_formula, design, na.rm = TRUE)
        weighted_var_overall <- svyvar(var_formula, design, na.rm = TRUE)
        weighted_stats_strata <- svyby(var_formula, strata_formula, design, svymean, na.rm = TRUE)
        weighted_var_strata <- svyby(var_formula, strata_formula, design, svyvar, na.rm = TRUE)
      }
      row_data <- data.frame(Variable = "", Level = "Mean (SD)", stringsAsFactors = FALSE)
      if (mode %in% c("mixed", "weighted")) {
        val <- sprintf("%.2f (%.2f)", weighted_mean_overall, sqrt(weighted_var_overall))
      } else {
        val <- sprintf("%.2f (%.2f)", unweighted_mean_overall, unweighted_sd_overall)
      }
      row_data$Overall <- val
      for (i in seq_along(strata_levels)) {
        s_lvl <- strata_levels[i]
        if (mode %in% c("mixed", "weighted")) {
          mean_val <- weighted_stats_strata[i, var]
          sd_val <- sqrt(weighted_var_strata[i, var])
          val <- sprintf("%.2f (%.2f)", mean_val, sd_val)
        } else {
          val <- sprintf("%.2f (%.2f)", unweighted_mean_strata[s_lvl], unweighted_sd_strata[s_lvl])
        }
        row_data[[s_lvl]] <- val
      }
      results_list[[length(results_list) + 1]] <- row_data
    }
  }

  final_table <- do.call(rbind, results_list)
  return(final_table)
}

