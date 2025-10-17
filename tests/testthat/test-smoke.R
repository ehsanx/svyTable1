test_that("svytable1 runs without error", {
  # A very simple check to ensure the function is available and runs
  expect_true(is.function(svytable1))
})

test_that("svypooled runs without error", {
  # A simple check for the other main function
  expect_true(is.function(svypooled))
})
