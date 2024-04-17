parameters <- list(
  list(trt_effect = 1, alpha_uz = 1, beta_uy = 1),
  list(trt_effect = 2, alpha_uz = 0.5, beta_uy = 0.2),
  list(trt_effect = 1, alpha_uz = -0.5, beta_uy = 0.2),
  list(trt_effect = 1, alpha_uz = 0.5, beta_uy = -0.2),
  list(trt_effect = 1, alpha_uz = -0.5, beta_uy = -0.2)
)

for (params in parameters) {
  trt_effect <- params$trt_effect
  alpha_uz <- params$alpha_uz
  beta_uy <- params$beta_uy

  run_simulation <- function(seed) {
    data <- simulate_data(
      N = 1000, alpha_uz = 1, beta_uy = 1,
      seed = seed, treatment_effects = 1
    )

    return(bayesian_causens(data, "Z", "Y", c("X.1", "X.2", "X.3"), output_trace = TRUE))
  }

  simulated_ates <- unlist(lapply(1:10, run_simulation))
}
