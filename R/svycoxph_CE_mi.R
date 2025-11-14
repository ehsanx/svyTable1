# --- 1. THE FUNCTION DEFINITION ---

#' @title Test Constant Effect (PH) Assumption for Pooled svycoxph Models
#' @description
#' This function provides a valid alternative to \code{cox.zph()} for complex survey
#' designs with multiple imputation. It takes a multiply-imputed survey design
#' object that has been split into time intervals (using \code{survival::survSplit}) and
#' fits a separate pooled model for each interval. It generates a plot of the
#' coefficients over time and provides tidy summary tables to guide model
#' selection.
#'
#' @param formula_rhs A string for the RHS of the Cox model (e.g., "var + age").
#' @param design_split A \code{svyimputationList} object, created from data that
#'                     has *already been split* using \code{survSplit()}.
#' @param var_to_test A string for the exact coefficient name to plot
#'                    (e.g., "active_asthma" or "age").
#' @param tgroup_var A string for the time-interval variable created by
#'                   \code{survSplit} (default is "tgroup").
#' @param time_var A string for the original 'stop' time variable in the
#'                 split data (default is "followup" or "stime").
#' @param status_var A string for the event status variable
#'                   (default is "death" or "status").
#' @param main_model_fit A pooled \code{svycoxph} fit object (class \code{mipo}). This is the
#'                       main, un-split model. It is optional, but highly
#'                       recommended, as the function will print its tidy
#'                       summary for you.
#' @param print_split_summary A logical. If TRUE, prints a tidy summary table
#'                            of all pooled coefficients from all interval models.
#' @param ... Other arguments passed to plotting, such as \code{title},
#'            \code{xlab}, \code{ylab}, \code{add_smoother} (TRUE/FALSE),
#'            \code{use_classic_theme} (TRUE/FALSE), or
#'            \code{show_null_effect} (TRUE/FALSE).
#'
#' @details
#' \strong{How to Interpret the Results:}
#'
#' This function helps you decide which model to report in your paper.
#'
#' 1.  \strong{Examine the "Time-Varying Effect Plot":}
#'     * \strong{If the plot is flat:} The confidence intervals for all time
#'         intervals should substantially overlap. This indicates that the effect
#'         of your variable is \strong{constant} over time.
#'     * \strong{If the plot is sloped:} A clear, non-random trend (upward or
#'         downward) suggests the effect is \strong{not constant}. The statistical
#'         test for this is to check if the confidence intervals from
#'         different time points (e.g., the first and last) do *not* overlap.
#'
#' 2.  \strong{Choose Your Final Model:}
#'     * \strong{If the assumption is MET (plot is flat):} You should report the
#'         \strong{"Tidy Table for Final Paper (if Constant Effect Assumption is Met)"}.
#'         This single, pooled model is valid, more precise, and the
#'         correct model to report.
#'     * \strong{If the assumption is VIOLATED (plot is sloped):} You
#'         \strong{must not} report the main model. It is statistically invalid.
#'         The correct model to report is the time-split model, which is
#'         summarized in the \strong{"Tidy Summary of All Time-Interval Models"}
#'         table.
#'
#' @return A \code{ggplot} object showing the coefficient trend over time.
#'
#' @importFrom mice pool
#' @importFrom dplyr n
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # --- 1. Load Libraries ---
#' library(svyTable1)
#' library(dplyr)
#' library(survival)
#' library(survey)
#' library(mitools)
#' library(mice)
#' library(ggplot2)
#' library(knitr)
#'
#' # --- 2. Load data and create base design ---
#' data(nhanes_mortality)
#' analytic_design <- svydesign(
#'   strata = ~strata,
#'   id = ~psu,
#'   weights = ~survey_weight,
#'   data = nhanes_mortality,
#'   nest = TRUE
#' )
#'
#' # --- 3. Create analysis data.frame and INDUCE MISSINGNESS in COVARIATES ---
#' set.seed(123)
#' data_with_miss <- analytic_design$variables %>%
#'   filter(stime > 0) %>%
#'   mutate(
#'     # Create the exposure variable (complete)
#'     caff_bin = factor(
#'       ifelse(caff == "No consumption", "No", "Yes"),
#'       levels = c("No", "Yes")
#'     ),
#'     # Induce 10% missingness in 'age' (a confounder)
#'     age = ifelse(runif(n()) < 0.10, NA, age),
#'
#'     # Induce 15% MAR missingness in 'bmi_cat' (a confounder)
#'     bmi_cat = factor(ifelse(age > 50 & runif(n()) < 0.15, NA, as.character(bmi.cat)))
#'   )
#'
#' print("--- Missingness Induced in Covariates ---")
#' print(mice::md.pattern(data_with_miss[, c("age", "bmi_cat", "caff_bin", "stime", "status")],
#'                        plot=FALSE))
#'
#' # --- 4. Add Nelson-Aalen hazard (auxiliary variable for MICE) ---
#' data_with_miss$nelson_aalen <- nelsonaalen(
#'   data_with_miss,
#'   time = stime,
#'   status = status
#' )
#'
#' # --- 5. Run MICE ---
#' print("--- Starting MICE ---")
#' M_IMPUTATIONS <- 5
#' MAX_ITERATIONS <- 5
#'
#' pred_matrix <- make.predictorMatrix(data_with_miss)
#' vars_to_keep_as_is <- c("id", "survey.weight", "psu", "strata",
#'                         "stime", "status", "nelson_aalen", "caff_bin", "sex")
#' pred_matrix[, vars_to_keep_as_is] <- 0
#' pred_matrix[vars_to_keep_as_is, ] <- 0
#'
#' imputed_data <- mice(
#'   data_with_miss,
#'   m = M_IMPUTATIONS,
#'   maxit = MAX_ITERATIONS,
#'   predictorMatrix = pred_matrix,
#'   method = 'pmm',
#'   seed = 123,
#'   printFlag = FALSE
#' )
#' print("--- MICE Complete ---")
#'
#' # --- 6. Create Stacked Long-Format Data ---
#' impdata_long <- mice::complete(imputed_data, "long", include = FALSE)
#'
#' # --- 7. Fit the MAIN (Constant Effect) Model ---
#' print("--- Fitting Main Pooled (Constant Effect) Model ---")
#'
#' allImputations_main <- imputationList(split(impdata_long, impdata_long$.imp))
#' design_main <- svydesign(
#'   strata = ~strata,
#'   id = ~psu,
#'   weights = ~survey_weight,
#'   data = allImputations_main,
#'   nest = TRUE
#' )
#'
#' my_formula <- "caff_bin + sex + age + bmi_cat"
#' main_formula <- as.formula(paste0("Surv(stime, status) ~ ", my_formula))
#'
#' main_fit_list <- with(design_main, svycoxph(main_formula))
#' main_fit_pooled <- pool(main_fit_list)
#'
#' # --- 8. Create the SPLIT-TIME Design ---
#' print("--- Creating Split-Time Design ---")
#' event_times <- data_with_miss$stime[data_with_miss$status == 1]
#' cuts <- quantile(event_times, probs = c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
#'
#' impdata_long_split <- survSplit(Surv(stime, status) ~ .,
#'                                 data = impdata_long,
#'                                 cut = cuts,
#'                                 episode = "tgroup",
#'                                 id = "split_id")
#'
#' allImputations_split <- imputationList(split(impdata_long_split,
#'                                              impdata_long_split$.imp))
#'
#' design_split <- svydesign(
#'   strata = ~strata,
#'   id = ~psu,
#'   weights = ~survey_weight,
#'   data = allImputations_split,
#'   nest = TRUE
#' )
#'
#' # --- 9. Run the PH Test Function and Print the Plot ---
#' print("--- Calling svycoxph_CE_mi function to test 'caff_binYes' ---")
#'
#' my_ph_plot <- svycoxph_CE_mi(
#'   formula_rhs = my_formula,
#'   design_split = design_split,
#'   var_to_test = "caff_binYes", # Test the exposure
#'   tgroup_var = "tgroup",
#'   time_var = "stime",
#'   status_var = "status",
#'   main_model_fit = main_fit_pooled, # Pass the main model here
#'   print_split_summary = TRUE,
#'   show_null_effect = TRUE
#' )
#'
#' # Explicitly print the plot
#' print(my_ph_plot)
#'
#' } # End \dontrun{}

svycoxph_CE_mi <- function(formula_rhs,
                           design_split,
                           var_to_test,
                           tgroup_var = "tgroup",
                           time_var = "followup",
                           status_var = "death",
                           main_model_fit = NULL,
                           print_split_summary = TRUE,
                           ...) {

  # --- 1. Extract data and formula components ---
  data_clean <- design_split$designs[[1]]$variables # Use first imputation for setup

  # Check for required split columns
  if (!tgroup_var %in% names(data_clean)) {
    stop(paste("Error: tgroup_var '", tgroup_var, "' not found in design object.", sep=""))
  }
  if (!"tstart" %in% names(data_clean)) {
    stop("Error: 'tstart' column not found. Was this design created with survSplit?")
  }

  tgroups <- sort(unique(data_clean[[tgroup_var]]))
  n_intervals <- length(tgroups)

  # --- 2. Print Main (PH) Model (if provided) ---
  if (!is.null(main_model_fit)) {
    print("--- Tidy Table for Final Paper (if Constant Effect Assumption is Met) ---")

    # Check if it's a pooled object
    if ("mipo" %in% class(main_model_fit)) {
      sum_fit <- summary(main_model_fit, all.comp = TRUE, conf.int = TRUE)

      tidy_main_model <- data.frame(
        Variable = sum_fit$term,
        HR = exp(sum_fit$estimate), # FIX: Convert coef to HR
        `CI.Lower` = sum_fit$conf.low,
        `CI.Upper` = sum_fit$conf.high,
        `p.value` = sum_fit$p.value,
        row.names = NULL,
        check.names = FALSE
      )
      print(knitr::kable(tidy_main_model, digits = 3, format = "pipe"))
    } else {
      print("Warning: 'main_model_fit' was not a pooled (mipo) object. Skipping summary.")
    }
    cat("\n\n")
  }

  # --- 3. Loop, Fit Pooled Models, and Extract Coefficients ---
  print(paste("--- Running Constant Effect Check: Using", n_intervals, "intervals ---"))

  plot_results_list <- list()
  summary_results_list <- list()

  loop_formula <- as.formula(
    paste0("Surv(tstart, ", time_var, ", ", status_var, ") ~ ", formula_rhs)
  )

  for (i in tgroups) {

    # Subset the pooled design
    design_subset <- subset(design_split, eval(as.name(tgroup_var)) == i)

    # Fit the model m times and pool the results
    fit_list <- with(design_subset, svycoxph(loop_formula))
    fit_pooled <- pool(fit_list)

    sum_fit <- summary(fit_pooled, all.comp = TRUE, conf.int = TRUE)
    coefs <- summary(fit_pooled, all.comp = TRUE, conf.int = FALSE)

    # --- Extract data for the Tidy Summary Table ---
    if (print_split_summary) {

      # Get interval times for label
      tstart_val <- min(design_subset$designs[[1]]$variables$tstart)
      tstop_val <- max(design_subset$designs[[1]]$variables[[time_var]])

      tidy_summary <- data.frame(
        Time.Interval = paste0(round(tstart_val, 0), "-", round(tstop_val, 0), " units"),
        Variable = sum_fit$term,
        HR = exp(sum_fit$estimate), # FIX: Convert coef to HR
        `CI.Lower` = sum_fit$conf.low,
        `CI.Upper` = sum_fit$conf.high, # FIX: Typo (.9G -> .95)
        `p.value` = sum_fit$p.value,
        row.names = NULL,
        check.names = FALSE
      )
      summary_results_list[[i]] <- tidy_summary
    }

    # --- Extract data for the Plot ---
    var_index <- which(coefs$term == var_to_test)
    if (length(var_index) > 0) {
      coef <- coefs$estimate[var_index]
      se <- coefs$std.error[var_index]
    } else {
      coef <- 0
      se <- 0
    }

    plot_results_list[[i]] <- data.frame(
      tgroup = i,
      coef = coef,
      se = se
    )
  }

  # --- 4. Print Tidy Summary Table (if requested) ---
  if (print_split_summary) {
    full_summary_df <- do.call(rbind, summary_results_list)
    print("--- Tidy Summary of All Time-Interval Models (if Constant Effect is Violated) ---")
    print(knitr::kable(full_summary_df, digits = 3, format = "pipe"))
    cat("\n\n")
  }

  # --- 5. Combine and Plot the Results ---
  results_df <- do.call(rbind, plot_results_list)

  if (is.null(results_df) || nrow(results_df) == 0) {
    stop("All models failed to converge or the variable was not found.")
  }

  results_df <- results_df %>%
    mutate(
      time = 1:n(), # Use simple 1, 2, 3... for x-axis
      ci_low = .data$coef - 1.96 * .data$se,
      ci_high = .data$coef + 1.96 * .data$se
    )

  print("--- Estimated Coefficients over Time (for Plot) ---")
  print(knitr::kable(results_df, digits = 3, format = "pipe"))

  # --- 6. Build the Plot ---
  # Capture optional plotting arguments
  plot_args <- list(...)
  plot_title <- plot_args$title %||% paste("Time-Varying Effect Plot:", var_to_test)
  plot_xlab <- plot_args$xlab %||% "Time Interval"
  plot_ylab <- plot_args$ylab %||% paste("Log-Hazard Ratio (Coefficient) for", var_to_test)
  use_classic_theme <- plot_args$use_classic_theme %||% TRUE
  show_null_effect <- plot_args$show_null_effect %||% FALSE
  add_smoother <- plot_args$add_smoother %||% FALSE

  plot_theme <- if (use_classic_theme) theme_classic() else theme_minimal()

  p <- ggplot(results_df, aes(x = time, y = coef)) +
    geom_point(size = 3) +
    geom_line() +
    geom_errorbar(aes(ymin = .data$ci_low, ymax = .data$ci_high), width = 0.2) +
    plot_theme +
    labs(title = plot_title, x = plot_xlab, y = plot_ylab) +
    scale_x_continuous(breaks = 1:n_intervals, labels = 1:n_intervals)

  if (show_null_effect) {
    p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "red")
  }

  if (add_smoother) {
    p <- p + geom_smooth(method = "loess", se = FALSE, linetype = "dotted", color = "blue")
  }

  return(p)
}

# --- Internal helper for ... arguments ---
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
