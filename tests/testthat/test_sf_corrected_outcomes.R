library(testthat)
library(causens)

# create a mock treatment model object
trt_model <- lm(
  outcome ~ confounder1 + confounder2,
  data = data.frame(
    outcome = c(0, 1, 0, 1),
    confounder1 = c(1, 0, 1, 0),
    confounder2 = c(0, 1, 0, 1)
  )
)

# create a test data frame
test_data <- data.frame(
  exposure = c(0, 1, 0, 1),
  outcome = c(0, 1, 0, 1),
  confounder1 = c(1, 0, 1, 0),
  confounder2 = c(0, 1, 0, 1)
)

# create a test for the corrected_outcomes function
test_that("corrected_outcomes returns the correct output", {
  # define the sensitivity function
  sf <- function(z, e) {
    if (z == 0) {
      return(0.5)
    } else {
      return(1.5)
    }
  }

  # call the function
  corrected <- corrected_outcomes(
    trt_model,
    test_data,
    "exposure",
    "outcome",
    c("confounder1", "confounder2"),
    sf
  )

  # check the output
  expect_equal(corrected, c(0.25, 1.25, -0.25, 0.75))
})
