library(testthat)

z <- c(1, 0, 1, 1, 0)
e <- c(1, 0, 1, 1, 0)
c1 <- 0.5
c0 <- 0.3
s1 <- 0.1
s0 <- 0.2
result <- list(
  "constant" = c(0.5, 0.3, 0.5, 0.5, 0.3),
  "linear" = c(0.6, 0.3, 0.6, 0.6, 0.3)
)


for (i in 1:5) { # 5 test cases for each form
  test_that("sf works", {
    expect_equal(
      sf(z[i], e[i], "constant", c1, c0),
      result[["constant"]][i]
    )
    expect_equal(
      sf(z[i], e[i], "linear", c1, c0, s1, s0),
      result[["linear"]][i]
    )
  })
}
