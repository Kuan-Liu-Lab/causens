# Analysis with all confounders (X, U) being measured. Later on, we assume that
# U could be missing.

trt_effect <- 1
<<<<<<< HEAD
data <- simulate_data(
  N = 10000, alpha_uz = 1, beta_uy = 1,
  treatment_effects = trt_effect
)
=======
data <- simulate_data(N = 10000, alpha_uz = 1, beta_uy = 1,
                      treatment_effects = trt_effect)
>>>>>>> 9718988 (Justifying c1, c0 choices in simulation)

# Implementing PSweight's utility from scratch...

trt_model <- glm(Z ~ X.1 + X.2 + X.3 + U, family = binomial(), data = data)
e <- predict(trt_model, type = "response")
weights <- (1 / e) * data$Z + (1 / (1 - e)) * (1 - data$Z)
Y1_hat <- sum((data$Y * weights)[data$Z == 1]) / sum(weights[data$Z == 1])
Y0_hat <- sum((data$Y * weights)[data$Z == 0]) / sum(weights[data$Z == 0])

ate_correct_estimate <- Y1_hat - Y0_hat

library(PSweight)

# Using PSweight

ate_correct <- PSweight( # adjusts for confounder U
  ps.formula = Z ~ X.1 + X.2 + X.3 + U,
  yname = "Y",
  family = "binomial",
  weight = "IPW",
  ps.method = "glm",
  data = data
)

test_that("Adjusting for U yields correct ATE estimates", {
  expect_equal(ate_correct_estimate, summary(ate_correct)$estimates[1])
  expect_equal(summary(ate_correct)$estimates[1], trt_effect, tolerance = 0.05)
})

ate_incorrect <- PSweight( # adjusts for confounder U
  ps.formula = Z ~ X.1 + X.2 + X.3,
  yname = "Y",
  family = "binomial",
  weight = "IPW",
  ps.method = "glm",
  data = data
)

test_that("Not adjusting for U yields incorrect ATE estimates", {
  expect_false(summary(ate_incorrect)$estimates[1] == trt_effect)
})

# What if U were missing?
# First, consider that we have potential outcomes Y(1) and Y(0)
# which, by the fundamental problem of causal inference, we don't

run_simulation <- function(seed) {
  data <- simulate_data(
    N = 1000, alpha_uz = 1, beta_uy = 1,
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
  Y0_Z1 <- predict(u_model, newdata = data.frame(subset(X, data$Z == 1), Z = 1))
  Y0_Z0 <- predict(u_model, newdata = data.frame(subset(X, data$Z == 1), Z = 0))
  c0 <- y0_model$coefficients["U"] * (mean(Y0_Z1) - mean(Y0_Z0))

  # Below, we conduct the data analysis assuming U is unmeasured
  # but we have values of c1, c0 such that our ATE estimate is consistent.

  trt_model <- glm(Z ~ X.1 + X.2 + X.3, family = binomial(), data = data)

  e <- predict(trt_model, type = "response") # propensity score

  weights <- 1 / ifelse(data$Z, e, 1 - e)
  Y_sf <- data$Y + (-1)**(data$Z == 1) * abs(data$Z - e) * ifelse(data$Z, c1, c0)

  Y1_sf <- sum((Y_sf * weights)[data$Z == 1]) / sum(weights[data$Z == 1])
  Y0_sf <- sum((Y_sf * weights)[data$Z == 0]) / sum(weights[data$Z == 0])

  estimated_ate <- Y1_sf - Y0_sf

  expect_equal(
    estimated_ate,
    causens(
      glm(Z ~ X.1 + X.2 + X.3, family = binomial(), data = data),
      data = data,
      exposure = "Z",
      outcome = "Y",
      c1 = c1,
      c0 = c0
    )
  )

  return(estimated_ate)
}

# Because alpha_uz > 0 and beta_uy > 0, treated individuals are more likely to
# have a better outcome, i.e. both potential outcomes Y(1) and Y(0). Hence, we
# need c1 > 0 and c0 > 0.
# Here, c1 = 0.25 and c0 = 0.25 seem to be most valid choices. It is hard to
# determine the exact numerical value of c1 and c0, but their magnitude can be.
simulated_ates <- c()

for (iter in 1:1000) {
  simulated_ates <- c(simulated_ates, run_simulation(iter))
}

test_that("Simulation retrieves ATE with 'correct' c1, c0 values", {
  expect_equal(mean(simulated_ates), trt_effect, tolerance = 0.01)
})
