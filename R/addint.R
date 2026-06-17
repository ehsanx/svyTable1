#' Calculate Additive Interaction Measures (RERI, AP, S)
#'
#' @description
#' Calculates measures of additive interaction (Relative Excess Risk due to
#' Interaction - RERI, Attributable Proportion due to interaction - AP,
#' and Synergy Index - S) with delta method confidence intervals. It can
#' use coefficients from either a joint variable model or an interaction term
#' model (`svycoxph`, `svyglm`, `coxph`, `glm`). It correctly handles the
#' variance-covariance matrix from survey design objects.
#'
#' @details
#' The function extracts coefficients and the variance-covariance matrix from the
#' fitted model. Based on the `type` specified ('joint' or 'interaction'), it
#' selects the appropriate coefficients corresponding to the exposure levels
#' provided in `coef_names`. It then uses the delta method via the `msm::deltamethod`
#' function to calculate the standard errors for RERI, AP, and log(S), and
#' constructs confidence intervals.
#'
#' RERI = RR11 - RR10 - RR01 + 1 (or ORs/HRs)
#' AP = RERI / RR11
#' S = (RR11 - 1) / ((RR10 - 1) + (RR01 - 1))
#'
#' Confidence intervals for S are calculated on the log scale and then exponentiated.
#' By default the RERI interval uses the symmetric delta-method (Wald) interval;
#' set \code{ci_method = "mover"} for the MOVER interval of Zou (2008)
#' <doi:10.1093/aje/kwn104>, which generally has better coverage.
#'
#' \strong{Scale caveat:} these measures are defined on the risk-ratio scale.
#' When the model supplies odds ratios (logistic) or hazard ratios (Cox), RERI,
#' AP, and S only approximate their risk-ratio counterparts if the outcome is
#' rare; for a common outcome the odds ratio overstates the risk ratio and the
#' measures can be biased. To target risk ratios directly, fit a log-binomial or
#' Poisson working model (see VanderWeele and Knol (2014)
#' <doi:10.1515/em-2013-0005>). The delta-method Wald intervals here are
#' symmetric and approximate.
#'
#' @param model A fitted model object (e.g., `svycoxph`, `svyglm`).
#' @param type Character string: `"joint"` if using a combined categorical variable,
#'   `"interaction"` if using main effects and a product term.
#' @param coef_names A list containing the exact names of the coefficients
#'   required for the calculation, based on the model `type`:
#'   \itemize{
#'     \item If `type = "joint"`: `list(exp1_level = "coef_name_A_only", exp2_level = "coef_name_B_only", both_levels = "coef_name_A_and_B")`
#'     \item If `type = "interaction"`: `list(exp1_coef = "coef_name_mainA", exp2_coef = "coef_name_mainB", inter_coef = "coef_name_A_x_B")`
#'   }
#'   Coefficient names must match `names(coef(model))` exactly.
#' @param measures A character vector specifying which measures to calculate.
#'   Options: `"RERI"`, `"AP"`, `"S"`, or `"all"`. Default is `"all"`.
#' @param conf.level Confidence level for the interval (default 0.95).
#' @param ci_method Character string giving the confidence-interval method for
#'   RERI: `"delta"` (default) for the symmetric delta-method Wald interval, or
#'   `"mover"` for the MOVER interval of Zou (2008)
#'   <doi:10.1093/aje/kwn104>, which recovers the variance from the component
#'   risk ratios' confidence limits and generally has better coverage. The AP and
#'   S intervals always use the delta method.
#'
#' @return A list containing named vectors for each requested measure.
#'   Each vector includes Estimate, SE (SE for RERI and AP, SE_log for S),
#'   LowerCI, UpperCI. Returns `NULL` or a partial list with `NA` values if
#'   calculation fails (e.g., missing coefficients, delta method error).
#'
#' @importFrom stats coef vcov qnorm pnorm na.omit cov2cor
#' @importFrom msm deltamethod
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(nhanes_mortality, package = "svyTable1")
#' nhanes_mortality$htn01 <- as.numeric(nhanes_mortality$htn == "Yes")
#'
#' design <- survey::svydesign(
#'   id = ~psu, strata = ~strata, weights = ~survey_weight,
#'   nest = TRUE, data = nhanes_mortality
#' )
#'
#' # Interaction-term model: additive interaction of sex and insulin use.
#' fit <- survey::svyglm(htn01 ~ sex * insulin + age,
#'                       design = design, family = quasibinomial())
#'
#' # Coefficient names must match names(coef(fit)) exactly.
#' addint(
#'   model = fit,
#'   type = "interaction",
#'   coef_names = list(
#'     exp1_coef  = "sexFemale",
#'     exp2_coef  = "insulinYes",
#'     inter_coef = "sexFemale:insulinYes"
#'   ),
#'   measures = "all"
#' )
#'
#' # RERI/AP/S can also be computed from a joint-variable model with
#' # type = "joint" (see the 'coef_names' argument). For logistic models these
#' # use odds ratios, which approximate risk ratios only when the outcome is rare.
#' }
addint <- function(model,
                   type = c("joint", "interaction"),
                   coef_names,
                   measures = "all",
                   conf.level = 0.95,
                   ci_method = c("delta", "mover")) {

  type <- match.arg(type)
  ci_method <- match.arg(ci_method)

  if (!requireNamespace("msm", quietly = TRUE)) {
    stop("Package 'msm' needed for this function to work. Please install it.", call. = FALSE)
  }

  valid_measures <- c("RERI", "AP", "S", "all")
  if (any(!measures %in% valid_measures)) {
    stop("Invalid measure specified. Choose from 'RERI', 'AP', 'S', or 'all'.", call. = FALSE)
  }
  if ("all" %in% measures) {
    measures <- c("RERI", "AP", "S")
  }
  measures <- unique(measures)

  beta <- tryCatch(stats::coef(model), error = function(e) NULL)
  V <- tryCatch(stats::vcov(model), error = function(e) NULL)

  if (is.null(beta) || is.null(V)) {
    warning("Could not extract coefficients or vcov from the model.")
    return(NULL)
  }

  coef_names_all <- names(beta)
  results_list <- list()

  # --- Define required coefficients and formulas based on model type ---
  if (type == "joint") {
    req_names_list <- c("exp1_level", "exp2_level", "both_levels")
    if (!all(req_names_list %in% names(coef_names))) {
      stop("For type='joint', coef_names list must contain: ", paste(req_names_list, collapse=", "), call.=FALSE)
    }
    coef_10 <- coef_names$exp1_level
    coef_01 <- coef_names$exp2_level
    coef_11 <- coef_names$both_levels

    required_coefs <- c(coef_10, coef_01, coef_11)

    # Formulas in terms of log-ORs/log-HRs from joint model
    reri_formula_str <- "~ exp(x3) - exp(x1) - exp(x2) + 1" # x1=b10, x2=b01, x3=b11
    ap_formula_str   <- "~ (exp(x3) - exp(x1) - exp(x2) + 1) / exp(x3)"
    log_s_formula_str<- "~ log(exp(x3) - 1) - log(exp(x1) + exp(x2) - 2)"

  } else if (type == "interaction") {
    req_names_list <- c("exp1_coef", "exp2_coef", "inter_coef")
    if (!all(req_names_list %in% names(coef_names))) {
      stop("For type='interaction', coef_names list must contain: ", paste(req_names_list, collapse=", "), call.=FALSE)
    }
    coef_A <- coef_names$exp1_coef
    coef_B <- coef_names$exp2_coef
    coef_AB <- coef_names$inter_coef

    required_coefs <- c(coef_A, coef_B, coef_AB)

    # Formulas in terms of log-ORs/log-HRs from interaction model
    reri_formula_str <- "~ exp(x1 + x2 + x3) - exp(x1) - exp(x2) + 1" # x1=bA, x2=bB, x3=bAB
    ap_formula_str   <- "~ (exp(x1 + x2 + x3) - exp(x1) - exp(x2) + 1) / exp(x1 + x2 + x3)"
    log_s_formula_str<- "~ log(exp(x1 + x2 + x3) - 1) - log(exp(x1) + exp(x2) - 2)"

  } else {
    stop("Invalid model type specified.", call. = FALSE) # Should be caught by match.arg
  }

  # --- Check if coefficients exist in the model ---
  missing_coefs <- required_coefs[!required_coefs %in% coef_names_all]
  if (length(missing_coefs) > 0) {
    warning("The following coefficients were not found in the model: ",
            paste(missing_coefs, collapse = ", "), ". Returning NULL for this combination.")
    return(NULL) # Return NULL instead of stopping if only some coefs missing
  }

  # --- Get indices and select relevant beta/V ---
  involved_indices <- match(required_coefs, coef_names_all)
  selected_beta <- beta[involved_indices]
  selected_V <- V[involved_indices, involved_indices, drop = FALSE] # Ensure it remains a matrix

  # Check if selected_V is valid
  if(any(is.na(selected_beta)) || any(is.infinite(selected_beta)) ||
     any(is.na(selected_V)) || any(is.infinite(selected_V)) ||
     length(selected_beta) != length(required_coefs) ||
     !all(dim(selected_V) == c(length(required_coefs), length(required_coefs)))) {
    warning("Invalid coefficients or variance-covariance matrix elements selected. Returning NULL.")
    return(NULL)
  }

  # --- Calculate Point Estimates ---
  est_reri <- est_ap <- est_s <- NA
  hr10 <- hr01 <- hr11 <- NA # Initialize for safety checks

  tryCatch({
    if (type == "joint") {
      b10 <- beta[coef_10]; b01 <- beta[coef_01]; b11 <- beta[coef_11]
      hr10 <- exp(b10); hr01 <- exp(b01); hr11 <- exp(b11)
    } else { # interaction
      bA <- beta[coef_A]; bB <- beta[coef_B]; bAB <- beta[coef_AB]
      hr10 <- exp(bA); hr01 <- exp(bB); hr11 <- exp(bA + bB + bAB)
    }

    # Ensure HRs are valid numbers before proceeding
    if(any(is.na(c(hr10, hr01, hr11))) || any(is.infinite(c(hr10, hr01, hr11)))) {
      stop("Calculated HR/ORs resulted in NA or Inf.")
    }

    est_reri <- hr11 - hr10 - hr01 + 1
    est_ap <- ifelse(abs(hr11) < 1e-10, NA, est_reri / hr11)
    denom_s <- (hr10 - 1) + (hr01 - 1)
    est_s <- ifelse(abs(denom_s) < 1e-10, NA, (hr11 - 1) / denom_s)
  }, error = function(e) {
    warning("Error calculating point estimates (possibly due to exp overflow or invalid inputs): ", e$message)
    # Ensure all estimates are NA if error occurs
    est_reri <<- est_ap <<- est_s <<- NA
    hr10 <<- hr01 <<- hr11 <<- NA
  })

  alpha <- 1 - conf.level
  z <- stats::qnorm(1 - alpha / 2)

  # --- Calculate RERI if requested ---
  if ("RERI" %in% measures) {
    se_reri <- NA
    lower_ci <- NA
    upper_ci <- NA
    if (!is.na(est_reri)) {
      se_reri <- tryCatch({
        msm::deltamethod(as.formula(reri_formula_str), selected_beta, selected_V)
      }, error = function(e) {
        warning("Delta method calculation failed for RERI: ", e$message)
        NA
      })
      if (!is.na(se_reri)) {
        lower_ci <- est_reri - z * se_reri
        upper_ci <- est_reri + z * se_reri
      }
      # Optionally replace the symmetric Wald interval with the MOVER interval
      # (Zou 2008), which recovers the variance from the component risk ratios'
      # confidence limits and generally has better coverage for RERI.
      if (identical(ci_method, "mover")) {
        mover <- tryCatch(
          .reri_mover_ci(selected_beta, selected_V, type, z, est_reri),
          error = function(e) {
            warning("MOVER interval failed for RERI; keeping the Wald interval: ",
                    e$message)
            NULL
          })
        if (!is.null(mover) && all(is.finite(mover))) {
          lower_ci <- mover[1]
          upper_ci <- mover[2]
        }
      }
    }
    results_list$RERI <- c(Estimate = est_reri, SE = se_reri, LowerCI = lower_ci, UpperCI = upper_ci)
    names(results_list$RERI) <- c("RERI_Estimate", "RERI_SE", paste0("RERI_CI", floor(conf.level*100), "_low"), paste0("RERI_CI", floor(conf.level*100), "_upp"))
  }

  # --- Calculate AP if requested ---
  if ("AP" %in% measures) {
    se_ap <- NA
    lower_ci <- NA
    upper_ci <- NA
    if (is.na(est_ap)) {
      warning("HR11 (derived or direct) is close to zero or point estimate calculation failed, AP cannot be calculated.")
    } else {
      se_ap <- tryCatch({
        msm::deltamethod(as.formula(ap_formula_str), selected_beta, selected_V)
      }, error = function(e) {
        warning("Delta method calculation failed for AP: ", e$message)
        NA
      })
      if (!is.na(se_ap)) {
        lower_ci <- est_ap - z * se_ap
        upper_ci <- est_ap + z * se_ap
      }
    }
    results_list$AP <- c(Estimate = est_ap, SE = se_ap, LowerCI = lower_ci, UpperCI = upper_ci)
    names(results_list$AP) <- c("AP_Estimate", "AP_SE", paste0("AP_CI", floor(conf.level*100), "_low"), paste0("AP_CI", floor(conf.level*100), "_upp"))
  }

  # --- Calculate S if requested ---
  if ("S" %in% measures) {
    se_log_s <- NA
    lower_ci <- NA
    upper_ci <- NA
    if (is.na(est_s)) {
      warning("Denominator for S is close to zero or point estimate calculation failed, Synergy Index cannot be reliably calculated.")
    } else {
      se_log_s <- tryCatch({
        # Ensure arguments to log are positive (use small tolerance)
        if (is.na(hr11) || is.na(hr10) || is.na(hr01) || (hr11 - 1) <= 1e-10 || (hr10 + hr01 - 2) <= 1e-10) {
          stop("Cannot calculate log(S) because derived HR/OR components are not sufficiently positive.")
        }
        msm::deltamethod(as.formula(log_s_formula_str), selected_beta, selected_V)
      }, error = function(e) {
        warning("Delta method calculation failed for log(S): ", e$message)
        NA
      })

      if (!is.na(se_log_s)) {
        if (is.na(est_s) || est_s <= 1e-10) { # Check point estimate again
          warning("Point estimate for S is not sufficiently positive, log(S) CI cannot be calculated.")
        } else {
          log_est_s <- log(est_s)
          lower_ci_log <- log_est_s - z * se_log_s
          upper_ci_log <- log_est_s + z * se_log_s
          lower_ci <- exp(lower_ci_log)
          upper_ci <- exp(upper_ci_log)
        }
      }
    }
    results_list$S <- c(Estimate = est_s, SE_log = se_log_s, LowerCI = lower_ci, UpperCI = upper_ci)
    names(results_list$S) <- c("S_Estimate", "S_SE_log", paste0("S_CI", floor(conf.level*100), "_low"), paste0("S_CI", floor(conf.level*100), "_upp"))
  }

  return(results_list)
}

# Internal: MOVER (Method Of Variance Estimates Recovery) confidence interval for
# RERI = RR11 - RR10 - RR01 + 1, following Zou (2008) <doi:10.1093/aje/kwn104>.
# `selected_beta`/`selected_V` are the three model coefficients and their 3x3
# variance-covariance matrix, in the order implied by `type`. Returns
# c(lower, upper). Not exported.
.reri_mover_ci <- function(selected_beta, selected_V, type, z, est_reri) {
  # Contrast matrix mapping the coefficients to (log RR11, log RR10, log RR01).
  if (identical(type, "interaction")) {
    # selected_beta = (bA, bB, bAB): RR10 = exp(bA), RR01 = exp(bB),
    # RR11 = exp(bA + bB + bAB).
    C <- rbind(c(1, 1, 1), c(1, 0, 0), c(0, 1, 0))
  } else {
    # joint: selected_beta = (b10, b01, b11): RR10 = exp(b10), RR01 = exp(b01),
    # RR11 = exp(b11).
    C <- rbind(c(0, 0, 1), c(1, 0, 0), c(0, 1, 0))
  }

  log_rr <- as.numeric(C %*% as.numeric(selected_beta))
  Sigma  <- C %*% selected_V %*% t(C)
  se     <- sqrt(diag(Sigma))
  if (any(!is.finite(se)) || any(se <= 0)) return(NULL)

  R <- stats::cov2cor(Sigma)
  r12 <- R[1, 2]; r13 <- R[1, 3]; r23 <- R[2, 3]   # (RR11,RR10),(RR11,RR01),(RR10,RR01)

  theta <- exp(log_rr)                              # RR11, RR10, RR01
  l <- exp(log_rr - z * se)
  u <- exp(log_rr + z * se)

  # RERI = theta1 - theta2 - theta3 + 1 (a1 = +1, a2 = a3 = -1).
  var_lower <- (theta[1] - l[1])^2 + (u[2] - theta[2])^2 + (u[3] - theta[3])^2 -
    2 * r12 * (theta[1] - l[1]) * (u[2] - theta[2]) -
    2 * r13 * (theta[1] - l[1]) * (u[3] - theta[3]) +
    2 * r23 * (u[2] - theta[2]) * (u[3] - theta[3])
  var_upper <- (u[1] - theta[1])^2 + (theta[2] - l[2])^2 + (theta[3] - l[3])^2 -
    2 * r12 * (u[1] - theta[1]) * (theta[2] - l[2]) -
    2 * r13 * (u[1] - theta[1]) * (theta[3] - l[3]) +
    2 * r23 * (theta[2] - l[2]) * (theta[3] - l[3])

  c(est_reri - sqrt(max(0, var_lower)), est_reri + sqrt(max(0, var_upper)))
}
