#' svyTable1: Survey-Weighted Tables and Diagnostics for Epidemiology
#'
#' @description
#' A teaching toolkit for analyzing complex survey data (such as 'NHANES'). The
#' exported functions are organized into the following groups.
#'
#' @section Survey design:
#' \itemize{
#'   \item \code{\link{svydesign_build}}: build a survey design from plain column
#'     names, with input validation and correct subpopulation handling.
#' }
#'
#' @section Descriptive tables:
#' \itemize{
#'   \item \code{\link{svytable1}}: survey-weighted "Table 1" with optional NCHS
#'     reliability suppression.
#' }
#'
#' @section Model diagnostics:
#' \itemize{
#'   \item \code{\link{svydiag}}: per-coefficient reliability diagnostics.
#'   \item \code{\link{svygof}}: Archer-Lemeshow goodness-of-fit test.
#'   \item \code{\link{svyAUC}}: design-correct AUC.
#' }
#'
#' @section Interaction analysis:
#' \itemize{
#'   \item \code{\link{addint}}, \code{\link{addintlist}}: additive interaction
#'     (RERI, AP, S).
#'   \item \code{\link{jointeffects}}, \code{\link{inteffects}}: joint and simple
#'     (stratum-specific) effects.
#'   \item \code{\link{reportint}}: multi-panel interaction report.
#'   \item \code{\link{plotint}}: interaction plots for continuous moderators.
#' }
#'
#' @section Survival analysis:
#' \itemize{
#'   \item \code{\link{svykmplot}}: survey-weighted Kaplan-Meier plots.
#'   \item \code{\link{svycoxph_CE}}, \code{\link{svycoxph_CE_mi}}:
#'     constant-effect / proportional-hazards diagnostics.
#' }
#'
#' @section Multiple imputation:
#' \itemize{
#'   \item \code{\link{svypooled}}: fallacy-safe pooled tables from \pkg{mice}.
#' }
#'
#' @section Data:
#' \itemize{
#'   \item \code{\link{nhanes_mortality}}: bundled NHANES mortality cohort used
#'     throughout the examples and vignettes.
#' }
#'
#' @keywords internal
"_PACKAGE"
