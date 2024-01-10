library(testthat)

test_cases_constant <- list(
  z = c(1, 0, 1, 1, 0),
  e = c(1, 0, 1, 1, 0),
  form = "constant",
  c1 = 0.5,
  c0 = 0.3,
  expected = c(0.5, 0.3, 0.5, 0.5, 0.3)
)

test_cases_linear <- list(
  z = c(1, 0, 1, 1, 0),
  e = c(1, 0, 1, 1, 0),
  form = "linear",
  c1 = 0.5,
  c0 = 0.3,
  s1 = 0.1,
  s0 = 0.2,
  expected = c(0.6, 0.3, 0.6, 0.6, 0.3)
)

for (test_cases in list(test_cases_constant, test_cases_linear)) {
  test_that("sf works", {
    expect_equal(
      sf(
        z = test_cases$z,
        e = test_cases$e,
        form = test_cases$form,
        c1 = test_cases$c1,
        c0 = test_cases$c0,
        s1 = test_cases$s1,
        s0 = test_cases$s0
      ),
      test_cases$expected
    )
  })
}
