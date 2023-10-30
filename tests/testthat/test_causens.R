context("causens function")

# Load the necessary packages
library(causens)
library(testthat)

# Define a test dataset
data <- data.frame(
  exposure = c(0, 1, 0, 1, 0, 1, 0, 1),
  outcome = c(0, 1, 0, 1, 0, 1, 0, 1),
  covariate1 = c(1, 2, 3, 4, 5, 6, 7, 8),
  covariate2 = c(8, 7, 6, 5, 4, 3, 2, 1)
)

# Define the outcome and treatment models
out_model <- glm(
  outcome ~ covariate1 + covariate2,
  data = data,
  family = binomial(link = "logit")
)
trt_model <- glm(
  exposure ~ covariate1 + covariate2,
  data = data,
  family = binomial(link = "logit")
)

# Define the expected output
expected_output <- glm(
  outcome ~ exposure,
  data = data,
  family = binomial(link = "logit")
)

# Test the causens function
test_that("causens function returns the expected output", {
  output <- causens(out_model, trt_model, data, "exposure", "outcome")
  expect_equal(output$coefficients, expected_output$coefficients)
})
