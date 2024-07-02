if (Sys.getenv("SKIP_BAYESIAN_SA_TESTS") == "true") {
  testthat::skip("Skipping Bayesian SA tests due to environment setting")
}

trt_effect <- 1

run_simulation <- function(seed) {
  data <- simulate_data(
    N = 1000, alpha_uz = 0.5, beta_uy = 0.2,
    seed = seed, treatment_effects = trt_effect
  )

  return(causens(Z ~ X.1 + X.2 + X.3, "Y", data, method = "Bayesian"))
}

simulated_ate <- c()

for (seed in 1:5) {
  simulated_ate <- c(simulated_ate, run_simulation(seed))
}

expect_equal(trt_effect, mean(simulated_ate), tolerance = 0.1)
