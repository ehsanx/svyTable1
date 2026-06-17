test_that("svydesign_build returns a design equal to a manual svydesign", {
  d <- st1_data()
  db <- svydesign_build(d, ids = "psu", strata = "strata",
                        weights = "survey_weight", verbose = FALSE)
  expect_s3_class(db, "survey.design2")

  manual <- survey::svydesign(id = ~psu, strata = ~strata,
                              weights = ~survey_weight, nest = TRUE, data = d)
  expect_equal(as.numeric(survey::svymean(~age, db)),
               as.numeric(survey::svymean(~age, manual)), tolerance = 1e-9)
})

test_that("svydesign_build subpop matches survey::subset (not pre-filtering)", {
  d <- st1_data()
  db_sub <- svydesign_build(d, ids = "psu", strata = "strata",
                            weights = "survey_weight",
                            subpop = "sex == 'Female'", verbose = FALSE)
  full <- survey::svydesign(id = ~psu, strata = ~strata,
                            weights = ~survey_weight, nest = TRUE, data = d)
  ref <- subset(full, sex == "Female")
  expect_equal(as.numeric(survey::svymean(~age, db_sub)),
               as.numeric(survey::svymean(~age, ref)), tolerance = 1e-9)
})

test_that("svydesign_build validates its inputs", {
  d <- st1_data()
  expect_error(svydesign_build(d, ids = "nope", weights = "survey_weight"),
               "not found")
  expect_error(svydesign_build(d, ids = "psu", weights = "sex"), "numeric")
  expect_error(svydesign_build(list(a = 1), ids = "psu", weights = "w"),
               "data.frame")

  d_neg <- d; d_neg$survey_weight[1] <- -1
  expect_error(svydesign_build(d_neg, ids = "psu", weights = "survey_weight"),
               "negative")

  d_na <- d; d_na$survey_weight[1] <- NA
  expect_error(svydesign_build(d_na, ids = "psu", weights = "survey_weight"),
               "missing")

  d_zero <- d; d_zero$survey_weight <- 0
  expect_error(svydesign_build(d_zero, ids = "psu", weights = "survey_weight"),
               "zero")

  expect_error(
    svydesign_build(d, ids = "psu", strata = "strata",
                    weights = "survey_weight", subpop = "not_a_col == 1"),
    "subpop"
  )
})

test_that("svydesign_build reports the analytic sample size", {
  d <- st1_data()
  expect_message(
    svydesign_build(d, ids = "psu", strata = "strata",
                    weights = "survey_weight"),
    "3780 rows"
  )
})
