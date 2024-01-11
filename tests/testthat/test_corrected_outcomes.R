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
  trt_model <- glm(exposure ~ confounder1 + confounder2, data = test_data, family = binomial())
  # call the function
  corrected <- corrected_outcomes(
    trt_model,
    test_data,
    test_data$exposure,
    test_data$outcome,
    form = "constant",
    c1 = 0.5,
    c0 = 0.3
  )
})
