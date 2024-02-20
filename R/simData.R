#' @title Generate data with unmeasured confounder
#'
#' @param ymodel A string indicating the functional form of the outcome model.
#' @param N The number of observations to be generated.
#' @param u_type A string indicating the type of the unmeasured confounder: "binary" or "continuous".
#' @param y_type A string indicating the type of the outcome: "binary" or "continuous".
#' @param alpha_uz The coefficient of the unmeasured confounder in the propensity score model.
#' @param beta_uy The coefficient of the unmeasured confounder in the outcome model.
#' @param treatment_effects The treatment effect.
#' @param seed The seed for the random number generator.
#'
#' @return A data frame with the simulated dataset.
#'
#' @importFrom stats plogis predict rbinom rnorm
#' @export
simulate_data <- function(ymodel = "linear",
                          N = 500,
                          u_type = "binary",
                          y_type = "continuous",
                          seed = 123,
                          alpha_uz = 0.2,
                          beta_uy = 0.5,
                          treatment_effects = 1) {
  set.seed(seed)

  alpha_xz <- c(.1, -.5, .2) # coefficients of X in the treatment model;
  beta_xy <- c(0.25, 0.5, -0.5) # coefficients of X in the outcome model;
  tau <- treatment_effects

  X <- matrix(rnorm(3 * N), nrow = N, ncol = 3) # covariates iid from N(0,1);

  if (u_type == "binary") {
    U <- rbinom(N, 1, .5) # unmeasured confounder;
  } else if (u_type == "cont" || u_type == "continuous") {
    U <- rnorm(N, 0, 1) # unmeasured confounder;
  } else {
    stop("Invalid unmeasured confounder type.")
  }

  ps <- plogis(-0.1 + X %*% alpha_xz + alpha_uz * (U)) # true propensity score;
  Z <- rbinom(N, 1, ps) # treatment variable;

  if (ymodel == "linear") {
    linear_predictor <- X %*% beta_xy + beta_uy * (U)
  } else if (ymodel == "nonlinear") {
    linear_predictor <- X^2 %*% beta_xy + beta_uy * (U)^2
  } else {
    stop("Invalid outcome model.")
  }

  if (y_type == "binary") {
    Y0 <- rbinom(N, 1, plogis(linear_predictor))
    Y1 <- rbinom(N, 1, plogis(linear_predictor + tau))
  } else if (y_type == "cont" || y_type == "continuous") {
    epsilon <- rnorm(N, 0, 1) # error term;
    Y0 <- linear_predictor + epsilon
    Y1 <- linear_predictor + tau + epsilon
  } else {
    stop("Invalid outcome type.")
  }

  Y <- Y0 * (1 - Z) + Y1 * Z

  return(as.data.frame(list(X = X, Z = Z, Y = Y, Y0 = Y0, Y1 = Y1, U = U)))
}

#' @title Generate data with a binary unmeasured confounder and continuous outcome
#'
#' @param ymodel A string indicating the functional form of the outcome model.
#' @param N The number of observations to be generated.
#' @param alpha_uz The coefficient of the unmeasured confounder in the propensity score model.
#' @param beta_uy The coefficient of the unmeasured confounder in the outcome model.
#' @param treatment_effects The treatment effect.
#' @param seed The seed for the random number generator.
#'
#' @examples
#' fulldata <- gData_U_binary_Y_cont(
#'   ymodel = "linear",
#'   N = 500,
#'   alpha_uz = 0.2,
#'   beta_uy = 0.5,
#'   treatment_effects = 1,
#'   seed = 123
#' )
#' table(fulldata$Z)
#' @export
gData_U_binary_Y_cont <- function(ymodel = "linear",
                                  N = 500,
                                  alpha_uz = 0.2,
                                  beta_uy = 0.5,
                                  treatment_effects = 1,
                                  seed = 123) {
  return(simulate_data(
    ymodel = ymodel,
    N = N,
    u_type = "binary",
    y_type = "continuous",
    seed = seed,
    alpha_uz = alpha_uz,
    beta_uy = beta_uy,
    treatment_effects = treatment_effects
  ))
}

#' @title Generate data with a continuous unmeasured confounder and continuous outcome
#'
#' @param ymodel A string indicating the functional form of the outcome model.
#' @param N The number of observations to be generated.
#' @param alpha_uz The coefficient of the unmeasured confounder in the propensity score model.
#' @param beta_uy The coefficient of the unmeasured confounder in the outcome model.
#' @param treatment_effects The treatment effect.
#' @param seed The seed for the random number generator.
#'
#' @export
gData_U_cont_Y_cont <- function(ymodel = "linear",
                                N = 500,
                                alpha_uz = 0.2,
                                beta_uy = 0.5,
                                treatment_effects = 1,
                                seed = 123) {
  return(simulate_data(
    ymodel = ymodel,
    N = N,
    u_type = "continuous",
    y_type = "continuous",
    seed = seed,
    alpha_uz = alpha_uz,
    beta_uy = beta_uy,
    treatment_effects = treatment_effects
  ))
}

#' @title Generate data with a continuous unmeasured confounder and a binary outcome
#'
#' @param ymodel A string indicating the functional form of the outcome model.
#' @param N The number of observations to be generated.
#' @param alpha_uz The coefficient of the unmeasured confounder in the propensity score model.
#' @param beta_uy The coefficient of the unmeasured confounder in the outcome model.
#' @param treatment_effects The treatment effect.
#' @param seed The seed for the random number generator.
#'
#' @export
gData_U_cont_Y_binary <- function(ymodel = "linear",
                                  N = 500,
                                  alpha_uz = 0.2,
                                  beta_uy = 0.5,
                                  treatment_effects = 1,
                                  seed = 123) {
  return(simulate_data(
    ymodel = ymodel,
    N = N,
    u_type = "continuous",
    y_type = "binary",
    seed = seed,
    alpha_uz = alpha_uz,
    beta_uy = beta_uy,
    treatment_effects = treatment_effects
  ))
}

#' @title Generate data with a binary unmeasured confounder and binary outcome
#'
#' @param ymodel A string indicating the functional form of the outcome model.
#' @param N The number of observations to be generated.
#' @param alpha_uz The coefficient of the unmeasured confounder in the propensity score model.
#' @param beta_uy The coefficient of the unmeasured confounder in the outcome model.
#' @param treatment_effects The treatment effect.
#' @param seed The seed for the random number generator.
#'
#' @export
gData_U_binary_Y_binary <- function(ymodel = "linear",
                                    N = 500,
                                    alpha_uz = 0.2,
                                    beta_uy = 0.5,
                                    treatment_effects = 1,
                                    seed = 123) {
  return(simulate_data(
    ymodel = ymodel,
    N = N,
    u_type = "binary",
    y_type = "binary",
    seed = seed,
    alpha_uz = alpha_uz,
    beta_uy = beta_uy,
    treatment_effects = treatment_effects
  ))
}
