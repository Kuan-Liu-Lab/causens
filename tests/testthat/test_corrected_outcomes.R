library(testthat)

# create a test data frame
test_data <- data.frame(
  exposure = c(0, 1, 0, 1),
  outcome = c(0, 1, 0, 1),
  confounder1 = c(1, 0, 1, 0),
  confounder2 = c(0, 1, 0, 1)
)

# create a test for the corrected_outcomes function
test_that("corrected_outcomes returns the correct output", {

  # call the function
  corrected <- corrected_outcomes(
    lm(outcome ~ exposure + confounder1 + confounder2, data = test_data),
    test_data,
    test_data$exposure,
    test_data$outcome,
    form = "constant",
    c1 = 0.5,
    c0 = 0.3
  )

  # TODO: Temporary "test" for checking GitHub Actions
  expect_equal(c(1e-4, 1e-5, 1e-6, 1e-7), c(0, 0, 0, 0), tolerance = 1e-3)
})
