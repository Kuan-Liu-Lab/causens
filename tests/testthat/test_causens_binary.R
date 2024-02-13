parameters <- list(
  list(trt_effect = 1, alpha_uz = 1, beta_uy = 1),
  list(trt_effect = 2, alpha_uz = 0.5, beta_uy = 0.2),
  list(trt_effect = 1, alpha_uz = -0.5, beta_uy = 0.2),
  list(trt_effect = 1, alpha_uz = 0.5, beta_uy = -0.2),
  list(trt_effect = 1, alpha_uz = -0.5, beta_uy = -0.2)
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

    y1_z1_model <- glm(Y1 ~ U + X.1 + X.2 + X.3, data = subset(data, data$Z == 1), family = binomial())
    y1_z0_model <- glm(Y1 ~ U + X.1 + X.2 + X.3, data = subset(data, data$Z == 0), family = binomial())
    y1_z1 <- predict(y1_z1_model, type = "response", newdata = data)
    y1_z0 <- predict(y1_z0_model, type = "response", newdata = data)
    c1 <- log(mean(y1_z1) / mean(y1_z0))

    y0_z1_model <- glm(Y0 ~ U + X.1 + X.2 + X.3, data = subset(data, data$Z == 1), family = binomial())
    y0_z0_model <- glm(Y0 ~ U + X.1 + X.2 + X.3, data = subset(data, data$Z == 0), family = binomial())
    y0_z1 <- predict(y0_z1_model, type = "response", newdata = data)
    y0_z0 <- predict(y0_z0_model, type = "response", newdata = data)
    c0 <- log(mean(y0_z1) / mean(y0_z0))

    trt_model <- glm(Z ~ X.1 + X.2 + X.3, family = binomial(), data = data)

    return(causens(trt_model, data, "Z", "Y", method = "Li", c1 = c1, c0 = c0))
  }

  simulated_ates <- unlist(lapply(1:1000, run_simulation))

  test_that("Simulation retrieves ATE with 'correct' c1, c0 values", {
    expect_true(abs(mean(simulated_ates) - true_risk_diff) < 0.05)
  })
}
