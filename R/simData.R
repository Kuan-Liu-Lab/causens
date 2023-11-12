gData_U_binary_Y_cont <- function(ymodel = "linear",
                                  samplesize = 500,
                                  alpha_uz = 0.2,
                                  beta_uz = 0.5,
                                  treatment_effects = 1,
                                  seed = 123) {
  N <- samplesize
  alpha_uz <- alpha_uz
  beta_uy <- beta_uz
  alpha_xz <- c(.1, -.5, .2) # coefficients of X in the treatment model;
  beta_xy <- c(0.25, 0.5, -0.5) # coefficients of X in the outcome model;
  tau <- treatment_effects
  set.seed(seed)
  X <- matrix(rnorm(3 * N), N, 3) # covariates from N(0,1);
  U <- rbinom(N, 1, .5) # unmeasured confounder;
  ps <- plogis(-0.1 + X %*% alpha_xz + alpha_uz * (U)) # propensity score model;
  Z <- rbinom(N, 1, ps) # treatment variable;
  epsilony <- rnorm(N, 0, 1) # error term;
  if (ymodel == "linear") {
    Y0 <- X %*% beta_xy + beta_uy * (U) + epsilony # potential outcome(Z=0)
    Y1 <- X %*% beta_xy + beta_uy * (U) + tau + epsilony # potential outcome(Z=1)
    Y <- Y0 * (1 - Z) + Y1 * Z
  } else if (ymodel == "nonlinear") {
    Y0 <- X^2 %*% beta_xy + beta_uy * (U)^2 + epsilony # potential outcome(Z=0)
    Y1 <- X^2 %*% beta_xy + beta_uy * (U)^2 + tau + epsilony # potential outcome(Z=1)
    Y <- Y0 * (1 - Z) + Y1 * Z
  }
  list(X = X, Z = Z, Y = Y, Y0 = Y0, Y1 = Y1, U = U)
}

# fulldata <- as.data.frame(gData_U_binary_Y_cont(ymodel = "linear",
#                                          samplesize = 500,
#                                          alpha_uz = 0.2,
#                                          beta_uz = 0.5,
#                                          treatment_effects = 1,
#                                          seed=123))
# table(fulldata$Z)

gData_U_cont_Y_cont <- function(ymodel = "linear",
                                samplesize = 500,
                                alpha_uz = 0.5,
                                beta_uz = 0.5,
                                treatment_effects = 0.2,
                                seed = 456) {
  N <- samplesize
  alpha_uz <- alpha_uz
  beta_uy <- beta_uz
  alpha_xz <- c(.1, -.5, .2) # coefficients of X in the treatment model;
  beta_xy <- c(0.25, 0.5, -0.5) # coefficients of X in the outcome model;
  tau <- treatment_effects
  set.seed(seed)
  X <- matrix(rnorm(3 * N), N, 3) # covariates from N(0,1);
  U <- rnorm(N, 0, 1) # unmeasured confounder;
  ps <- plogis(-0.1 + X %*% alpha_xz + alpha_uz * (U)) # propensity score model;
  Z <- rbinom(N, 1, ps) # treatment variable;
  epsilony <- rnorm(N, 0, 1) # error term;
  if (ymodel == "linear") {
    Y0 <- X %*% beta_xy + beta_uy * (U) + epsilony # potential outcome(Z=0)
    Y1 <- X %*% beta_xy + beta_uy * (U) + tau + epsilony # potential outcome(Z=1)
    Y <- Y0 * (1 - Z) + Y1 * Z
  } else if (ymodel == "nonlinear") {
    Y0 <- X^2 %*% beta_xy + beta_uy * (U)^2 + epsilony # potential outcome(Z=0)
    Y1 <- X^2 %*% beta_xy + beta_uy * (U)^2 + tau + epsilony # potential outcome(Z=1)
    Y <- Y0 * (1 - Z) + Y1 * Z
  }
  list(X = X, Z = Z, Y = Y, Y0 = Y0, Y1 = Y1, U = U)
}



# fulldata <- as.data.frame(gData_U_cont_Y_cont(ymodel = "linear",
#                                          samplesize = 500,
#                                          alpha_uz = 0.2,
#                                          beta_uz = 0.5,
#                                          treatment_effects = 1,
#                                          seed=456))
# table(fulldata$Z)


gData_U_binary_Y_binary <- function(ymodel = "linear",
                                    samplesize = 500,
                                    alpha_uz = 0.2,
                                    beta_uz = 0.1,
                                    treatment_effects = -0.2,
                                    seed = 123) {
  N <- samplesize
  alpha_uz <- alpha_uz
  beta_uy <- beta_uz
  alpha_xz <- c(.1, -.5, .2) # coefficients of X in the treatment model;
  beta_xy <- c(0.1, 0.1, -0.1) # coefficients of X in the outcome model;
  tau <- treatment_effects
  set.seed(seed)
  X <- matrix(rnorm(3 * N), N, 3) # covariates from N(0,1);
  U <- rbinom(N, 1, .5) # unmeasured confounder;
  ps <- plogis(-0.1 + X %*% alpha_xz + alpha_uz * (U)) # propensity score model;
  Z <- rbinom(N, 1, ps) # treatment variable;
  if (ymodel == "linear") {
    pY0 <- plogis(X %*% beta_xy + beta_uy * (U)) # potential outcome(Z=0)
    pY1 <- plogis(X %*% beta_xy + beta_uy * (U) + tau) # potential outcome(Z=1)
    Y0 <- rbinom(N, 1, pY0)
    Y1 <- rbinom(N, 1, pY1)
    Y <- Y0 * (1 - Z) + Y1 * Z
  } else if (ymodel == "nonlinear") {
    pY0 <- plogis(X^2 %*% beta_xy + beta_uy * (U)^2) # potential outcome(Z=0)
    pY1 <- plogis(X^2 %*% beta_xy + beta_uy * (U)^2 + tau) # potential outcome(Z=1)
    Y0 <- rbinom(N, 1, pY0)
    Y1 <- rbinom(N, 1, pY1)
    Y <- Y0 * (1 - Z) + Y1 * Z
  }
  list(X = X, Z = Z, Y = Y, Y0 = Y0, Y1 = Y1, U = U, pY0 = pY0, pY1 = pY1)
}

# fulldata <- as.data.frame(gData_U_binary_Y_binary(ymodel = "linear",
#                                                 samplesize = 500,
#                                                 alpha_uz = 0.2,
#                                                 beta_uz = 0.1,
#                                                 treatment_effects = -0.2,
#                                                 seed=123))
# table(fulldata$Z)
# mean(fulldata$pY1 - fulldata$pY0)

gData_U_cont_Y_binary <- function(ymodel = "linear",
                                  samplesize = 500,
                                  alpha_uz = 0.5,
                                  beta_uz = 0.5,
                                  treatment_effects = 0.2,
                                  seed = 456) {
  N <- samplesize
  alpha_uz <- alpha_uz
  beta_uy <- beta_uz
  alpha_xz <- c(.1, -.5, .2) # coefficients of X in the treatment model;
  beta_xy <- c(0.25, 0.5, -0.5) # coefficients of X in the outcome model;
  tau <- treatment_effects
  set.seed(seed)
  X <- matrix(rnorm(3 * N), N, 3) # covariates from N(0,1);
  U <- rnorm(N, 0, 1) # unmeasured confounder;
  ps <- plogis(-0.1 + X %*% alpha_xz + alpha_uz * (U)) # propensity score model;
  Z <- rbinom(N, 1, ps) # treatment variable;
  if (ymodel == "linear") {
    pY0 <- plogis(X %*% beta_xy + beta_uy * (U)) # potential outcome(Z=0)
    pY1 <- plogis(X %*% beta_xy + beta_uy * (U) + tau) # potential outcome(Z=1)
    Y0 <- rbinom(N, 1, pY0)
    Y1 <- rbinom(N, 1, pY1)
    Y <- Y0 * (1 - Z) + Y1 * Z
  } else if (ymodel == "nonlinear") {
    pY0 <- plogis(X^2 %*% beta_xy + beta_uy * (U)^2) # potential outcome(Z=0)
    pY1 <- plogis(X^2 %*% beta_xy + beta_uy * (U)^2 + tau) # potential outcome(Z=1)
    Y0 <- rbinom(N, 1, pY0)
    Y1 <- rbinom(N, 1, pY1)
    Y <- Y0 * (1 - Z) + Y1 * Z
  }
  list(X = X, Z = Z, Y = Y, Y0 = Y0, Y1 = Y1, U = U, pY0 = pY0, pY1 = pY1)
}



# fulldata <- as.data.frame(gData_U_cont_Y_binary(ymodel = "linear",
#                                               samplesize = 500,
#                                               alpha_uz = 0.2,
#                                               beta_uz = 0.1,
#                                               treatment_effects = -0.2,
#                                               seed=456))
# table(fulldata$Z)
# mean(fulldata$pY1 - fulldata$pY0)
