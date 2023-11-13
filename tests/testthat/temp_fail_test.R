library(causens)
library(testthat)

test_that("Test temporary failed test to ensure that GitHub Actions can detect this", {
  expect_false(is.function(simulate_data))
})
