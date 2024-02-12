library(testthat)

# create a test data frame
test_data <- simulate_data()

# create a test for the corrected_outcomes function
test_that("corrected_outcomes returns the correct output", {
  trt_model <- glm(Z ~ X.1 + X.2 + X.3, data = test_data, family = binomial())
  # call the function
  expect_error(
    corrected_outcomes(
      trt_model,
      test_data,
      test_data$Z,
      test_data$Y,
      form = "constant",
      c1 = 0.5,
      c0 = 0.3
    ), NA
  )
  expect_error
})
