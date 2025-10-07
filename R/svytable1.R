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
    # --- FINAL FIX: Corrected typo from is.weighted to is_weighted ---
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

  for (current_var_name in table_vars) {
    var_formula <- as.formula(paste0("~", current_var_name))
    strata_formula <- as.formula(paste0("~", strata_var))

    var_header_row <- as.data.frame(setNames(as.list(c(current_var_name, rep("", ncol(header_row) - 1))), names(header_row)), check.names = FALSE)
    results_list[[length(results_list) + 1]] <- var_header_row

    current_var_vector <- design$variables[[current_var_name]]

    if (is.factor(current_var_vector)) {

      if (any(is.na(current_var_vector))) {
        original_levels <- levels(current_var_vector)
        var_as_char <- as.character(current_var_vector)
        var_as_char[is.na(var_as_char)] <- "Missing"
        design$variables[[current_var_name]] <- factor(
          var_as_char,
          levels = c(original_levels, "Missing")
        )
      }

      uw_counts_overall <- table(design$variables[[current_var_name]])
      uw_counts_strata <- table(design$variables[[current_var_name]], design$variables[[strata_var]])
      w_counts_overall <- svytable(var_formula, design, round = TRUE)
      w_counts_strata <- svytable(as.formula(paste0("~", current_var_name, "+", strata_var)), design, round = TRUE)
      w_pcts_overall <- svymean(var_formula, design, na.rm = FALSE)
      w_pcts_strata <- svyby(var_formula, strata_formula, design, svymean, na.rm = FALSE)

      for (lvl in levels(design$variables[[current_var_name]])) {
        row_data <- data.frame(Variable = "", Level = lvl, stringsAsFactors = FALSE)
        col_name_pct <- paste0(current_var_name, lvl)

        if (mode == "mixed") {
          row_data$Overall <- sprintf("%s (%.1f%%)", format_num(uw_counts_overall[lvl], FALSE), w_pcts_overall[col_name_pct] * 100)
        } else if (mode == "weighted") {
          row_data$Overall <- sprintf("%s (%.1f%%)", format_num(w_counts_overall[lvl], TRUE), w_pcts_overall[col_name_pct] * 100)
        } else { # unweighted
          unweighted_pct <- 100 * uw_counts_overall[lvl] / sum(uw_counts_overall)
          row_data$Overall <- sprintf("%s (%.1f%%)", format_num(uw_counts_overall[lvl], FALSE), unweighted_pct)
        }

        for (s_lvl in strata_levels) {
          pct_val <- w_pcts_strata[w_pcts_strata[, 1] == s_lvl, col_name_pct]
          if(length(pct_val) == 0 || is.na(pct_val)) pct_val <- 0

          if (mode == "mixed") {
            uw_count <- uw_counts_strata[lvl, s_lvl]
            row_data[[s_lvl]] <- sprintf("%s (%.1f%%)", format_num(uw_count, FALSE), pct_val * 100)
          } else if (mode == "weighted") {
            w_count <- w_counts_strata[lvl, s_lvl]
            row_data[[s_lvl]] <- sprintf("%s (%.1f%%)", format_num(w_count, TRUE), pct_val * 100)
          } else { # unweighted
            uw_count <- uw_counts_strata[lvl, s_lvl]
            unweighted_pct_strata <- 100 * uw_count / sum(uw_counts_strata[, s_lvl])
            if (is.nan(unweighted_pct_strata)) unweighted_pct_strata <- 0
            row_data[[s_lvl]] <- sprintf("%s (%.1f%%)", format_num(uw_count, FALSE), unweighted_pct_strata)
          }
        }
        results_list[[length(results_list) + 1]] <- row_data
      }
    } else if (is.numeric(current_var_vector)) {
      row_data <- data.frame(Variable = "", Level = "Mean (SD)", stringsAsFactors = FALSE)

      if (mode %in% c("mixed", "weighted")) {
        mean_overall_w <- svymean(var_formula, design, na.rm = TRUE)
        var_overall_w <- svyvar(var_formula, design, na.rm = TRUE)
        row_data$Overall <- sprintf("%.2f (%.2f)", mean_overall_w[1], sqrt(var_overall_w[1]))

        means_by_strata_w <- svyby(var_formula, strata_formula, design, svymean, na.rm = TRUE)
        vars_by_strata_w <- svyby(var_formula, strata_formula, design, svyvar, na.rm = TRUE)

        for(i in 1:nrow(means_by_strata_w)){
          s_lvl <- as.character(means_by_strata_w[i, 1])
          row_data[[s_lvl]] <- sprintf("%.2f (%.2f)", means_by_strata_w[i, 2], sqrt(vars_by_strata_w[i, 2]))
        }
      } else { # unweighted
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
  return(final_table)
}
