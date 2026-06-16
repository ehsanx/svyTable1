# ---- Shared fixtures for svyTable1 tests ----------------------------------

# Deterministic mock model exposing coef()/vcov(), used for the pure-math
# (Tier 1) additive-interaction tests so the expected numbers do not depend on
# the survey package's variance machinery.
coef.svymock <- function(object, ...) object$coefficients
vcov.svymock <- function(object, ...) object$vcov
.S3method("coef", "svymock", coef.svymock)
.S3method("vcov", "svymock", vcov.svymock)

make_mock <- function(beta, V) {
  if (is.null(dimnames(V))) dimnames(V) <- list(names(beta), names(beta))
  structure(list(coefficients = beta, vcov = V), class = "svymock")
}

# Analytic data + standard design on the BUNDLED dataset (no Suggests needed).
st1_data <- function() {
  e <- new.env()
  utils::data("nhanes_mortality", package = "svyTable1", envir = e)
  d <- e$nhanes_mortality
  d$htn01 <- as.numeric(d$htn == "Yes")
  d
}

st1_design <- function(data = st1_data()) {
  survey::svydesign(id = ~psu, strata = ~strata, weights = ~survey_weight,
                    nest = TRUE, data = data)
}

st1_fit <- function(design = st1_design()) {
  survey::svyglm(htn01 ~ age + sex + smoking, design = design,
                 family = stats::quasibinomial())
}

st1_int_fit <- function(design = st1_design()) {
  survey::svyglm(htn01 ~ sex * insulin + age, design = design,
                 family = stats::quasibinomial())
}
