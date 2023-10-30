library(testthat)
library(causens)

# Test the sensitivity function
test_that("sensitivity function returns expected output", {
  # Define input values
  x <- c(1, 2, 3, 4, 5)
  y <- c(2, 4, 6, 8, 10)
  # Define expected output
  expected_output <- 2
  # Call the function
  output <- sensitivity(x, y)
  # Check if the output matches the expected output
  expect_equal(output, expected_output)
})
