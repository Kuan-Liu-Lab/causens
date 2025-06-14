testthat::skip_on_cran()

parameters <- list(
  list(trt_effect = 1, alpha_uz = 0.5, beta_uy = 0.2),
  list(trt_effect = 1, alpha_uz = -0.5, beta_uy = 0.2),
  list(trt_effect = 1, alpha_uz = 0.5, beta_uy = -0.2),
  list(trt_effect = 1, alpha_uz = -0.5, beta_uy = -0.2),
  list(trt_effect = 2, alpha_uz = 0.5, beta_uy = 0.2)
)

for (params in parameters) {
  trt_effect <- params$trt_effect
  alpha_uz <- params$alpha_uz
  beta_uy <- params$beta_uy

  run_simulation <- function(seed) {
    data <- simulate_data(
      N = 1000, alpha_uz = alpha_uz, beta_uy = beta_uy,
      seed = seed, treatment_effects = trt_effect, y_type = "binary",
      informative_u = FALSE
    )

    result <- causens_monte_carlo("Y", "Z", c("X.1", "X.2", "X.3"), data)

    return(result$estimated_ate)
  }

  simulated_ate <- c()

  for (seed in 1:30) { # 100 previously, reducing test time slightly
    simulated_ate <- c(simulated_ate, run_simulation(seed))
  }

  test_that("Monte Carlo method returns correct ATE", {
    expect_equal(trt_effect, mean(simulated_ate), tolerance = 0.05)
  })
}
