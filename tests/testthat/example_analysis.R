library(PSweight)

# Analysis with "missing confounder" U

data <- simulate_data(N = 1000, alpha_uz = 2, beta_uy = 2)
ate_correct <- PSweight( # adjusting for "unmeasured" confounder
  ps.formula = Z ~ 1 + X.1 + X.2 + X.3 + U,
  yname = "Y",
  family = "binomial",
  weight = "IPW",
  ps.method = "glm",
  data = data,
)
head(data)
summary(ate_correct)$estimates[1]

ate_incorrect <- PSweight( # does *not* adjust for missing confounder
  ps.formula = Z ~ X.1 + X.2 + X.3,
  yname = "Y",
  family = "binomial",
  weight = "IPW",
  ps.method = "glm",
  data = data
)

summary(ate_incorrect)$estimates[1] # does not work

# What if U were missing?
# First, consider that we have potential outcomes Y(1) and Y(0)
# which, by the fundamental problem of causal inference, we don't

run_simulation <- function(seed) {

  data <- simulate_data(N = 1000, alpha_uz = 2, beta_uy = 2, seed = seed)

  X <- data[, c("X.1", "X.2", "X.3", "U")]

  model1 <- lm(Y1 ~ Z + X.1 + X.2 + X.3 + U, data = data)
  model0 <- lm(Y0 ~ Z + X.1 + X.2 + X.3 + U, data = data)

  Y1_hat <- predict(model1, newdata = data.frame(Z = 1, X))
  Y0_hat <- predict(model0, newdata = data.frame(Z = 0, X))

  true_c1 <- mean(Y1_hat[data$Z == 1]) - mean(Y1_hat[data$Z == 0])
  true_c0 <- mean(Y0_hat[data$Z == 1]) - mean(Y0_hat[data$Z == 0])

  c(true_c1, true_c0)

  trt_model <- glm(Z ~ X.1 + X.2 + X.3, family = binomial(), data = data)
  e <- predict(trt_model, type = "response") # propensity score

  weights <- 1 / ifelse(data$Z, e, 1 - e)
  Y_sf <- data$Y + (-1)**(data$Z == 1) * abs(data$Z - predict(trt_model, type = "response")) * ifelse(data$Z == 1, true_c1, true_c0)

  estimated_ate <- sum((Y_sf * weights)[data$Z == 1]) / sum(weights[data$Z == 1]) - sum((Y_sf * weights)[data$Z == 0]) / sum(weights[data$Z == 0])

  return(estimated_ate)
}

# run simulation, retrieve ATE after adjusting for unmeasured confounding
simulated_ates <- c()

for (iter in 1:1000){
  simulated_ates <- c(simulated_ates, run_simulation(iter))
}
mean(simulated_ates)
