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
      N = 1000, alpha_uz = alpha_uz, beta_uy = beta_uy,
      seed = seed, treatment_effects = trt_effect
    )

    # On retrieving c1, c0 according to our data-generating mechanisms

    u_model <- lm(U ~ Z + X.1 + X.2 + X.3, data = data)
    X <- data[, c("X.1", "X.2", "X.3")] # measured confounders

    y1_model <- lm(Y1 ~ U + X.1 + X.2 + X.3, data = subset(data, data$Z == 1))
    Y1_Z1 <- predict(u_model, newdata = data.frame(subset(X, data$Z == 1), Z = 1))
    Y1_Z0 <- predict(u_model, newdata = data.frame(subset(X, data$Z == 1), Z = 0))
    c1 <- y1_model$coefficients["U"] * (mean(Y1_Z1) - mean(Y1_Z0))

    y0_model <- lm(Y0 ~ U + X.1 + X.2 + X.3, data = subset(data, data$Z == 1))
    Y0_Z1 <- predict(u_model, newdata = data.frame(subset(X, data$Z == 0), Z = 1))
    Y0_Z0 <- predict(u_model, newdata = data.frame(subset(X, data$Z == 0), Z = 0))
    c0 <- y0_model$coefficients["U"] * (mean(Y0_Z1) - mean(Y0_Z0))

    # Below, we conduct the data analysis assuming U is unmeasured
    # but we have values of c1, c0 such that our ATE estimate is consistent.

    trt_model <- Z ~ X.1 + X.2 + X.3

    return(causens(trt_model, "Y", method = "Li", data = data, c1 = c1, c0 = c0))
  }

  # Because alpha_uz > 0 and beta_uy > 0, treated individuals are more likely to
  # have a better outcome, i.e. both potential outcomes Y(1) and Y(0). Hence, we
  # need c1 > 0 and c0 > 0.
  # Here, c1 = 0.25 and c0 = 0.25 seem to be most valid choices. It is hard to
  # determine the exact numerical value of c1 and c0, but their magnitude can be.
  # Run the simulation for each iteration and get the results as a list

  simulated_ates <- unlist(lapply(1:1000, run_simulation))

  test_that("Simulation retrieves ATE with 'correct' c1, c0 values", {
    expect_equal(mean(simulated_ates), trt_effect, tolerance = 0.01)
  })

  data <- simulate_data(
    N = 1000, alpha_uz = alpha_uz, beta_uy = beta_uy,
    seed = 1234, treatment_effects = trt_effect
  )

  # Testing `trt_model` input types
  trt_model <- Z ~ X.1 + X.2 + X.3

  est_ate_1 <- causens(trt_model, "Y", method = "Li", data = data, c1 = 0.25, c0 = 0.25)
  est_ate_2 <- causens(
    glm(trt_model, data = data, family = binomial()),
    "Y",
    method = "Li",
    data = data,
    c1 = 0.25,
    c0 = 0.25
  )

  test_that("trt_model can be a formula or fitted glm model", {
    expect_equal(est_ate_1, est_ate_2)
  })
}

# data generated from loop/test above
test_that("causens throws an error if `trt_model` input is invalid", {
  trt_model <- "Z ~ X.1 + X.2 + X.3"

  expect_error(
    object = causens(trt_model, "Y", method = "Li", data = data, c1 = 0.25, c0 = 0.25),
    regexp = "Treatment model must be a formula or a glm object."
  )
})

test_that("causens throws an error if `method` input is invalid", {
  expect_error(
    object = causens(Z ~ 1, "Y", method = "???", data = data, c1 = 0.25, c0 = 0.25),
    regexp = "Method not recognized or not implemented yet."
  )
})
