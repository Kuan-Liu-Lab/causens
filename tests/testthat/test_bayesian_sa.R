if (Sys.getenv("SKIP_BAYESIAN_SA_TESTS") == "true") {
  testthat::skip("Skipping Bayesian SA tests due to environment setting")
}

testthat::skip_on_cran()

trt_effect <- 1

run_simulation <- function(seed, y_type) {
  data <- simulate_data(
    N = 1000, alpha_uz = 0.5, beta_uy = 0.2,
    seed = seed, treatment_effects = trt_effect,
    y_type = y_type, informative_u = TRUE
  )

  result <- bayesian_causens(Z ~ X.1 + X.2 + X.3,
    Y ~ X.1 + X.2 + X.3,
    U ~ X.1 + X.2 + X.3,
    data = data
  )

  return(result$estimated_ate)
}

simulated_ate <- list("binary" = c(), "continuous" = c())

for (y_type in c("continuous", "binary")) {
  for (seed in 1:5) {
    simulated_ate[[y_type]] <- c(
      simulated_ate[[y_type]],
      run_simulation(seed, y_type)
    )
  }
}

expect_equal(trt_effect, mean(simulated_ate$`binary`), tolerance = 0.1)
expect_equal(trt_effect, mean(simulated_ate$`continuous`), tolerance = 0.1)

# Testing summary function
data <- simulate_data(
  N = 1000, alpha_uz = 0.5, beta_uy = 0.2,
  seed = 123, treatment_effects = 1,
  y_type = "continuous", informative_u = TRUE
)

result <- bayesian_causens(Z ~ X.1 + X.2 + X.3,
  Y ~ X.1 + X.2 + X.3,
  U ~ X.1 + X.2 + X.3,
  data = data
)

test_that("summary.bayesian_causens produces correct output", {
  expect_equal(capture_output(summary(result)), paste(
    "Treatment Model:",
    "Z ~ X.1 + X.2 + X.3 ",
    "",
    "Estimate     Std.Error    95% Credible Interval     ",
    "1.1          0.127        (0.836, 1.36)                  ",
    sep = "\n"
  ))
})
