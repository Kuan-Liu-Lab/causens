parameters <- list(
  list(alpha_uz = 0, beta_uy = 0, trt_effect = 1)
)

for (params in parameters) {

  data <- simulate_data(
    N = 1e6, y_type = "binary", alpha_uz = params$alpha_uz, beta_uy = params$beta_uy,
    seed = 1234, treatment_effects = params$trt_effect, informative_u = FALSE
  ) # seed greater than 1000 to avoid overlap with other tests

  # need to estimate true risk difference via Monte Carlo integration
  # since closed-form solution is unavailable to us
  true_risk_diff <- mean(data$Y1) - mean(data$Y0)

  run_simulation <- function(seed) {
    data <- simulate_data(
      N = 1000, y_type = "binary", alpha_uz = params$alpha_uz, beta_uy = params$beta_uy,
      seed = seed, treatment_effects = params$trt_effect, informative_u = FALSE
    )

    # On retrieving c1, c0 according to our data-generating mechanisms

    trt_model <- Z ~ X.1 + X.2 + X.3

    return(causens(trt_model, "Y", data, method = "Li", c1 = 0, c0 = 0))
  }

  simulated_ates <- unlist(lapply(1:1000, run_simulation))

  test_that("Simulation retrieves ATE with 'correct' c1, c0 values", {
    expect_true(abs(mean(simulated_ates) - true_risk_diff) < 0.05)
  })
}
