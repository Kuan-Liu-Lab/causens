# This file checks that our functionality aligns well with the well-established
# PSweight package. We also show that conventional causal inference methods
# yield incorrect estimates when unmeasured confounding is present.

trt_effect <- 1
data <- simulate_data(
  N = 10000, alpha_uz = 1, beta_uy = 1,
  treatment_effects = trt_effect
)

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
