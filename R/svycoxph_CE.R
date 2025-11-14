#' @title Plot Time-Varying Coefficients for a svycoxph Model
#' @description
#' This function provides a valid alternative to cox.zph() for complex survey
#' designs by fitting separate models to different time intervals and
#' plotting the resulting coefficients to check the Constant Effect
#' (Proportional Hazards) assumption.
#'
#' @param formula_rhs A string for the right-hand side of your Cox model.
#' @param design The original, un-split svydesign object.
#' @param var_to_test A string specifying the exact coefficient name to plot.
#' @param time_var A string for the follow-up time variable.
#' @param status_var A string for the event status variable (must be 0/1).
#' @param n_intervals The number of time intervals to create (default is 5).
#' @param verbose A logical. If TRUE, prints extra debugging information.
#' @param print_main_model A logical. If TRUE, prints a tidy summary of the
#'                         main (un-split) proportional hazards model.
#' @param print_split_summary A logical. If TRUE, prints a tidy summary table
#'                            of all coefficients from all interval models.
#' @param add_smoother A logical. If TRUE, adds a loess smooth line to the plot.
#' @param title A string. Custom title for the plot.
#' @param xlab A string. Custom x-axis label.
#' @param ylab A string. Custom y-axis label.
#' @param use_classic_theme A logical. If TRUE, uses theme_classic() (white bg,
#'                          no grid). If FALSE, uses theme_minimal().
#' @param show_null_effect A logical. If TRUE (default), draws the red dashed
#'                         line at y=0.
#'
#' @return A ggplot object.
#'
#' @importFrom survey svycoxph
#' @importFrom survival survSplit
#' @importFrom rlang .data
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr setdiff
#'
#' @export
#' @examples
#' \dontrun{
#' # --- 1. Load data from svyTable1 and create base design ---
#' require(svyTable1)
#' data(nhanes_mortality)
#' analytic_design <- svydesign(
#'   strata = ~strata,
#'   id = ~psu,
#'   weights = ~survey_weight,
#'   data = nhanes_mortality,
#'   nest = TRUE
#' )
#'
#' # --- 2. Create the analysis data.frame with all variables ---
#' # We filter and create 'caff_bin' in this data.frame
#' data_full_clean <- analytic_design$variables %>%
#'   dplyr::filter(stime > 0) %>%
#'   mutate(
#'     caff_bin = factor(
#'       ifelse(caff == "No consumption", "No", "Yes"),
#'       levels = c("No", "Yes")
#'     )
#'   )
#'
#' # --- 3. Create the FINAL analytic design object ---
#' # This new design object *contains* caff_bin, sex, and age
#' analytic_design_final <- svydesign(
#'   strata = ~strata,
#'   id = ~psu,
#'   weights = ~survey_weight,
#'   data = data_full_clean,
#'   nest = TRUE
#' )
#'
#' # --- 4. Define the model and variable to test ---
#' my_formula <- "caff_bin + age"
#' my_var <- "age"
#'
#' # --- 5. Run the function ---
#' svycoxph_CE(
#'   formula_rhs = my_formula,
#'   design = analytic_design_final,
#'   var_to_test = my_var,
#'   time_var = "stime",
#'   status_var = "status",
#'   n_intervals = 5,
#'   verbose = TRUE,
#'   print_main_model = TRUE,
#'   print_split_summary = TRUE
#' )
#' }

svycoxph_CE <- function(formula_rhs,
                        design,
                        var_to_test,
                        time_var = "stime",
                        status_var = "status",
                        n_intervals = 5,
                        verbose = FALSE,
                        print_main_model = TRUE,
                        print_split_summary = TRUE,
                        add_smoother = FALSE,
                        title = NULL,
                        xlab = NULL,
                        ylab = NULL,
                        use_classic_theme = TRUE,
                        show_null_effect = FALSE) {

  # --- 1. Extract data and clean ---
  data_full <- design$variables
  data_clean <- data_full %>%
    dplyr::filter(.data[[time_var]] > 0)

  model_vars <- all.vars(as.formula(paste0("~", formula_rhs)))

  if (verbose) {
    print("--- Debug: Checking for variables in initial design ---")
    vars_in_design <- names(data_clean)
    vars_missing <- setdiff(model_vars, vars_in_design)
    if (length(vars_missing) > 0) {
      stop(paste("Error: The following model variables are missing from the input design:",
                 paste(vars_missing, collapse=", ")))
    } else {
      print("All model variables are present in the initial design.")
    }
    print("-------------------------------------------------")
  }

  # --- 2. Fit the Main (Constant Effect) Model (if requested) ---
  main_model_formula <- as.formula(
    paste0("Surv(", time_var, ", ", status_var, ") ~ ", formula_rhs)
  )

  if (print_main_model) {
    print("--- Fitting Main (Constant Effect) Model (for Constant Effect-met scenario) ---")
    main_fit <- svycoxph(main_model_formula, design = design)
    sum_fit <- summary(main_fit)
    hrs <- sum_fit$conf.int
    coefs <- sum_fit$coefficients

    tidy_main_model <- data.frame(
      Variable = rownames(hrs),
      HR = hrs[, "exp(coef)"],
      `CI.Lower` = hrs[, "lower .95"],
      `CI.Upper` = hrs[, "upper .95"],
      `p.value` = coefs[, "Pr(>|z|)"],
      row.names = NULL,
      check.names = FALSE
    )
    print("--- Tidy Table for Final Paper (if Constant Effect Assumption is Met) ---")
    print(knitr::kable(tidy_main_model, digits = 3, format = "pipe"))
    cat("\n\n") # Add spacing
  }


  # --- 3. Define Cut-points Based on Event Time Quantiles ---
  event_times <- data_clean[[time_var]][data_clean[[status_var]] == 1]
  probs <- (1:(n_intervals - 1)) / n_intervals
  cuts <- quantile(event_times, probs = probs, na.rm = TRUE)

  print(paste("--- Running Constant Effect Check: Using", n_intervals, "intervals based on these cut-points ---"))
  print(cuts)

  # --- 4. Create (start, stop) data ---
  split_formula <- as.formula(paste0("Surv(", time_var, ", ", status_var, ") ~ ."))

  split_data <- survSplit(split_formula,
                          data = data_clean,
                          cut = cuts,
                          episode = "tgroup",
                          id = "internal_split_id")

  # --- 5. Create the new survey design object ---
  call <- design$call
  call$data <- split_data
  split_design <- eval(call, envir = environment())

  # --- 6. Loop, Fit Models, and Extract Coefficients ---
  plot_results_list <- list()
  summary_results_list <- list()
  time_intervals <- c(0, cuts, max(split_data[[time_var]]))

  loop_formula <- as.formula(
    paste0("Surv(tstart, ", time_var, ", ", status_var, ") ~ ", formula_rhs)
  )

  for (i in 1:n_intervals) {

    if (verbose) {
      print(paste("--- Debug: Fitting model for tgroup", i, "---"))
    }

    fit <- tryCatch({
      design_subset <- subset(split_design, eval(as.name("tgroup")) == i)
      svycoxph(loop_formula, design = design_subset)
    }, error = function(e) {
      if (verbose) {
        print(paste("Warning: Model for tgroup", i, "failed to converge:", e$message))
      }
      NULL
    })

    if (!is.null(fit)) {
      if (verbose) {
        print(paste("Model for tgroup", i, "converged successfully."))
      }

      sum_fit <- summary(fit)
      hrs <- sum_fit$conf.int
      coefs <- sum_fit$coefficients

      # --- Extract data for the Tidy Summary Table ---
      if (print_split_summary) {
        tidy_summary <- data.frame(
          Time.Interval = paste0(round(time_intervals[i], 0), "-", round(time_intervals[i+1], 0), " units"),
          Variable = rownames(hrs),
          N = sum_fit$n,
          Events = sum_fit$nevent,
          HR = hrs[, "exp(coef)"],
          `CI.Lower` = hrs[, "lower .95"],
          `CI.Upper` = hrs[, "upper .95"], # <-- TYPO FIX (was .9G)
          `p.value` = coefs[, "Pr(>|z|)"],
          row.names = NULL,
          check.names = FALSE
        )
        summary_results_list[[i]] <- tidy_summary
      }

      # --- Extract data for the Plot ---
      if (var_to_test %in% rownames(coefs)) {
        coef <- coefs[var_to_test, "coef"]
        se <- coefs[var_to_test, "robust se"]
      } else {
        coef <- 0
        se <- 0
      }

      plot_results_list[[i]] <- data.frame(
        tgroup = i,
        time = (time_intervals[i] + time_intervals[i+1]) / 2,
        coef = coef,
        se = se
      )
    }
  }

  # --- 7. Print Tidy Summary Table (if requested) ---
  if (print_split_summary) {
    full_summary_df <- do.call(rbind, summary_results_list)
    print("--- Tidy Summary of All Time-Interval Models (if Constant Effect Assumption is Violated) ---")
    print(knitr::kable(full_summary_df, digits = 3, format = "pipe"))
    cat("\n\n") # Add spacing
  }

  # --- 8. Combine and Plot the Results ---
  results_df <- do.call(rbind, plot_results_list)

  if (is.null(results_df) || nrow(results_df) == 0) {
    stop("All models failed to converge or the variable was not found.")
  }

  results_df <- results_df %>%
    mutate(
      ci_low = coef - 1.96 * se,
      ci_high = coef + 1.96 * se
    )

  print("--- Estimated Coefficients over Time (for Plot) ---")
  print(knitr::kable(results_df, digits = 3, format = "pipe"))

  # --- 9. Build the Plot ---
  plot_title <- if (is.null(title)) paste("Time-Varying Effect Plot:", var_to_test) else title
  plot_xlab <- if (is.null(xlab)) "Follow-up Time" else xlab
  plot_ylab <- if (is.null(ylab)) paste("Log-Hazard Ratio (Coefficient) for", var_to_test) else ylab

  plot_theme <- if (use_classic_theme) {
    theme_classic()
  } else {
    theme_minimal()
  }

  p <- ggplot(results_df, aes(x = time, y = coef)) +
    geom_point(size = 3) +
    geom_line() +
    geom_errorbar(aes(ymin = .data$ci_low, ymax = .data$ci_high), width = 10) +
    plot_theme +
    labs(title = plot_title, x = plot_xlab, y = plot_ylab)

  if (show_null_effect) {
    p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "red")
  }

  if (add_smoother) {
    p <- p + geom_smooth(se = FALSE, linetype = "dotted", color = "blue")
  }

  return(p)
}


# # --- 1. Load data and create base design ---
# require(svyTable1)
# data(nhanes_mortality)
# analytic_design <- svydesign(
#   strata = ~strata,
#   id = ~psu,
#   weights = ~survey_weight,
#   data = nhanes_mortality,
#   nest = TRUE
# )
#
# # --- 2. Create the analysis data.frame with all variables ---
# data_full_clean <- analytic_design$variables %>%
#   dplyr::filter(stime > 0) %>%
#   mutate(
#     caff_bin = factor(
#       ifelse(caff == "No consumption", "No", "Yes"),
#       levels = c("No", "Yes")
#     )
#   )
#
# # --- 3. Create the FINAL analytic design object ---
# analytic_design_final <- svydesign(
#   strata = ~strata,
#   id = ~psu,
#   weights = ~survey_weight,
#   data = data_full_clean,
#   nest = TRUE
# )
#
# # --- 4. Define the model and variable to test ---
# my_formula <- "caff_bin + age + sex"
# my_var <- "caff_bin"
#
# # --- 5. Run the function ---
# svycoxph_CE(
#   formula_rhs = my_formula,
#   design = analytic_design_final,
#   var_to_test = my_var,
#   time_var = "stime",
#   status_var = "status",
#   n_intervals = 5,
#   verbose = TRUE,
#   print_main_model = TRUE,
#   print_split_summary = TRUE
# )
