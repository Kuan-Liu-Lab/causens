if (Sys.getenv("SKIP_BAYESIAN_SA_TESTS") == "true") {
  testthat::skip("Skipping Bayesian SA tests due to environment setting")
}

trt_effect <- 1

run_simulation <- function(seed, y_type) {
  data <- simulate_data(
    N = 1000, alpha_uz = 0.5, beta_uy = 0.2,
    seed = seed, treatment_effects = trt_effect,
    y_type = y_type
  )

  return(causens(Z ~ X.1 + X.2 + X.3, "Y", data, method = "Bayesian"))
}

simulated_ate <- list("binary" = c(), "continuous" = c())

for (y_type in c("continuous", "binary")) {
  for (seed in 1:5) {
    simulated_ate[[y_type]] <- c(simulated_ate[[y_type]], run_simulation(seed, y_type))
  }
}

expect_equal(trt_effect, mean(simulated_ate$`binary`), tolerance = 0.1)
expect_equal(trt_effect, mean(simulated_ate$`continuous`), tolerance = 0.1)
