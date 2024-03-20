#' Tasks
#' 1. correct parameter naming
#' 2. make sure it doesn't crash
#' 3. run on simulated data

#' @title Bayesian parametric sensitivity analysis for causal inference
#' @description This function runs a Bayesian sensitivity analysis for causal
#' inference using JAGS or Stan as a backend.
#' @param data A data frame containing the exposure, outcome, and confounders.
#' @param exposure The name of the exposure variable in the data frame.
#' @param outcome The name of the outcome variable in the data frame.
#' @param confounders The name of the confounders in the data frame.
#' @param backend The backend to use for the sensitivity analysis. Currently
#' only "jags" is supported.
#' @param ... Additional arguments to be passed to the backend.
#' @return A list of posterior samples for the causal effect of the exposure
#' variable on the outcome, as well as the confounder-adjusted causal effect.
bayesian_causens <- function(data, exposure, outcome, confounders, backend = "jags", ...) {
  if (backend == "rjags" || backend == "jags") {
    library(rjags)
  } else if (backend == "stan" || backend == "rstan") {
    stop("Stan backend will be implemented soon.")
  } else {
    stop("Backend not recognized or not implemented yet.")
  }

  # Notation from Bayesian SA paper
  X <- data[[exposure]]
  C <- data[confounders]
  Y <- data[[outcome]]
  N <- nrow(C)

  jags_model <- create_jags_model()

  inits <- list(
    beta_X = 0,
    beta_C = rep(0, ncol(C)),
    beta_0 = 0,
    beta_U = 0,
    gamma_C = rep(0, ncol(C)),
    gamma_0 = 0
  )

  # Run the Bayesian sensitivity analysis
  model <- jags.model(
    textConnection(jags_model),
    data = list(
      X = X,
      C = C,
      Y = Y,
      N = N,
      p_unmeasured_confounder = ncol(C),
      p_outcome = ncol(C)
    ),
    inits = inits
  )

  update(model, 1000)

  # Extract the posterior samples
  samples <- coda.samples(
    model,
    variable.names = c("beta_X", "beta_C", "beta_0", "gamma_C", "gamma_0"),
    n.iter = 5000,
    thin = 1
  )

  return(mean(samples[[1]][, "beta_X"]))
}

parse_args <- function(...) {
  if (n_samples %in% names(args)) {
    n_samples <- args$n_samples
  } else {
    n_samples <- 10000
  }

  if (burn_in %in% names(args)) {
    burn_in <- args$burn_in
  } else {
    burn_in <- 1000
  }

  return(list(n_samples = n_samples, burn_in = burn_in))
}

#' @title Create an JAGS model for Bayesian sensitivity analysis
#' @description Creates a JAGS model available as a string, or .txt file, where
#' priors are initialized to be uninformative by default.
#'
#' No inputs are given to this function (for now) since data-related information
#' is provided in jags.model() during model initialization.
create_jags_model <- function() {
  priors <- "
  # Outcome model parameters

  for (o in 1:p_outcome) {
    beta[o] ~ dunif(-2, 2)
  }

  # Unmeasured Confounder parameters

  gamma_0 ~ dunif(-5, 5)

  gamma_C[1] ~ dunif(-5, 5)

  for (u in 2:p_unmeasured_confounder) {
    gamma_C[u] ~ dunif(-2, 2)
  }

  # Outcome model parameters

  beta_0 ~ dunif(-2, 2)
  beta_X ~ dnorm(0, 0.5)

  for (c in 1:p_outcome) {
    beta_C[c] ~ dunif(-2, 2)
  }

  beta_U ~ dunif(-2, 2)

  tau_Y ~ dgamma(0.1, 0.1)
  "

  # including modelling of unmeasured binary confounder (`eta` is the linear predictor)
  likelihood <- "
  for (i in 1:N) {
    mu_Y[i] <- beta_0 + beta_X * X[i] + sum(C[i, 1:p_outcome] * beta_C[1:p_outcome]) + beta_U * U[i]
    Y[i] ~ dnorm(mu_Y[i], tau_Y)
  }
  "

  unmeasured_confounder <- "
  for (i in 1:N) {
    logit(p_U[i]) <- gamma_0 + sum(C[i, 1:p_unmeasured_confounder] * gamma_C[1:p_unmeasured_confounder])
    U[i] ~ dbern(p_U[i])
  }
  "

  return(paste0(
    "model{\n",
    priors,
    likelihood,
    unmeasured_confounder,
    "\n}"
  ))
}
