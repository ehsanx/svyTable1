# svypooled renders a publication table from a mice::pool() (mipo) object.

make_pooled <- function() {
  d <- st1_data()
  da <- d[, c("htn01", "sex", "age", "smoking", "psu", "strata", "survey_weight")]
  set.seed(1)
  da$age[sample(nrow(da), 200)] <- NA
  imp <- mice::mice(da, m = 2, maxit = 2, printFlag = FALSE, seed = 1)
  fit_imp <- with(imp, survey::svyglm(
    htn01 ~ sex + age + smoking,
    design = survey::svydesign(id = ~psu, strata = ~strata,
                               weights = ~survey_weight, nest = TRUE,
                               data = data.frame(mget(ls()))),
    family = stats::quasibinomial()
  ))
  mice::pool(fit_imp)
}

test_that("svypooled returns a kableExtra/knitr_kable object (fallacy-safe)", {
  skip_if_not_installed("mice")
  tab <- svypooled(make_pooled(), main_exposure = "sex",
                   adj_var_names = c("age", "smoking"), measure = "OR",
                   title = "Test")
  expect_s3_class(tab, "knitr_kable")
  expect_s3_class(tab, "kableExtra")
})

test_that("svypooled full table also renders", {
  skip_if_not_installed("mice")
  tab <- svypooled(make_pooled(), main_exposure = "sex",
                   adj_var_names = c("age", "smoking"), measure = "OR",
                   fallacy_safe = FALSE)
  expect_s3_class(tab, "knitr_kable")
})

test_that("svypooled errors when the main exposure is absent", {
  skip_if_not_installed("mice")
  expect_error(
    svypooled(make_pooled(), main_exposure = "not_there",
              adj_var_names = c("age", "smoking")),
    "not found"
  )
})
