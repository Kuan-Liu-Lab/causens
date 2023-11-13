library(testthat)

test_cases_increasing <- list(
  z = c(1, 0, 1, 1, 0),
  e = c(0.1, 0.3, 0.5, 0.6, 0.95),
  form = "increasing",
  expected = c(0.0550, 0.0650, 0.0750, 0.0800, 0.0975)
)

test_cases_decreasing <- list(
  z = c(1, 0, 1, 1, 0),
  e = c(0.1, 0.3, 0.5, 0.6, 0.95),
  form = "decreasing",
  expected = c(0.0950, 0.0850, 0.0750, 0.0700, 0.0525)
)

for (test_cases in list(test_cases_increasing, test_cases_decreasing)) {
  test_that("sensitivity_function works", {
    expect_equal(
      sensitivity_function(
        z = test_cases$z,
        e = test_cases$e,
        form = test_cases$form
      ),
      test_cases$expected
    )
  })
}
