trt_effect <- 1

run_simulation <- function(seed) {
  data <- simulate_data(
    N = 1000, alpha_uz = 0.5, beta_uy = 0.2,
    seed = seed, treatment_effects = trt_effect, y_type = "binary",
    informative_u = FALSE
  )

  return(causens(Z ~ X.1 + X.2 + X.3, "Y", data, method = "Monte Carlo"))
}

simulated_ate <- c()

for (seed in 1:100) {
  simulated_ate <- c(simulated_ate, run_simulation(seed))
}

expect_equal(trt_effect, mean(simulated_ate), tolerance = 0.05)
