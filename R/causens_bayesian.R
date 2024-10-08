#' @title Bayesian parametric sensitivity analysis for causal inference
#' @description This function runs a Bayesian sensitivity analysis for causal
#' inference using JAGS or Stan as a backend.
#' @param exposure The name of the exposure variable in the data frame.
#' @param outcome The name of the outcome variable in the data frame.
#' @param confounders The name of the confounders in the data frame.
#' @param data A data frame containing the exposure, outcome, and confounder variables.
#' @param backend The backend to use for the sensitivity analysis. Currently
#' only "jags" is supported.
#' @param output_trace Whether to output the full trace of the MCMC sampler.
#' @param ... Additional arguments to be passed to the backend.
#' @return A list of posterior samples for the causal effect of the exposure
#' variable on the outcome, as well as the confounder-adjusted causal effect.
bayesian_causens <- function(exposure, outcome, confounders, data, backend = "jags", output_trace = FALSE, ...) {
  sampler_args <- parse_args(...)

  # Notation from Bayesian SA paper
  Z <- data[[exposure]]
  C <- data[confounders]
  Y <- data[[outcome]]
  N <- nrow(C)

  binary_outcome <- all(Y %in% c(0, 1))

  jags_model <- create_jags_model(binary_outcome)

  inits <- list(
    beta_Z = 0,
    beta_C = rep(0, ncol(C)),
    beta_0 = 0,
    beta_U = 0,
    gamma_C = rep(0, ncol(C)),
    alpha_C = rep(0, ncol(C)),
    alpha_0 = 0
  )

  if (backend == "rjags" || backend == "jags") {
    if (!requireNamespace("rjags", quietly = TRUE)) {
      stop("The 'rjags' package is required for the JAGS backend but is not
      installed. Please install it using install.packages('rjags').")
    }

    # Run the Bayesian sensitivity analysis using RJAGS
    model <- rjags::jags.model(
      textConnection(jags_model),
      data = list(
        Z = Z,
        C = C,
        Y = Y,
        N = N,
        p_outcome = ncol(C),
        p_treatment = ncol(C),
        p_unmeasured_confounder = ncol(C)
      ),
      inits = inits
    )

    update(model, sampler_args$burn_in)

    # Extract the posterior samples
    samples <- rjags::coda.samples(
      model,
      variable.names = c("beta_Z", "beta_C", "beta_0", "gamma_C", "alpha_C", "alpha_U", "alpha_0"),
      n.iter = sampler_args$n_samples,
      thin = 1
    )

    if (output_trace) {
      return(samples[[1]])
    }

    return(mean(samples[[1]][, "beta_Z"]))
  } else if (backend == "stan" || backend == "rstan") {
    stop("Stan backend will be implemented soon.")
  } else {
    stop("Backend not recognized or not implemented yet.")
  }
}

parse_args <- function(...) {
  args <- list(...)

  if ("n_samples" %in% names(args)) {
    n_samples <- args$n_samples
  } else {
    n_samples <- 20000
  }

  if ("burn_in" %in% names(args)) {
    burn_in <- args$burn_in
  } else {
    burn_in <- 5000
  }

  return(list(n_samples = n_samples, burn_in = burn_in))
}

#' @title Create an JAGS model for Bayesian sensitivity analysis
#' @description Creates a JAGS model available as a string, or .txt file, where
#' priors are initialized to be uninformative by default.
#' @param binary_outcome Boolean indicating whether the outcome is binary.
#'
#' No inputs are given to this function (for now) since data-related information
#' is provided in jags.model() during model initialization.
create_jags_model <- function(binary_outcome) {
  # including modelling of unmeasured binary confounder (`eta` is the linear predictor)
  likelihood <- "
  # Outcome model parameters

  beta_0 ~ dunif(-2, 2)
  beta_Z ~ dnorm(0, 0.5)

  for (c in 1:p_outcome) {
    beta_C[c] ~ dunif(-2, 2)
  }

  beta_U ~ dunif(-2, 2)
  "

  if (binary_outcome) {
    likelihood <- paste0(likelihood, "
    for (i in 1:N) {
      logit(p_Y[i]) <- beta_0 + beta_Z * Z[i] + sum(C[i, 1:p_outcome] * beta_C[1:p_outcome]) + beta_U * U[i]
      Y[i] ~ dbern(p_Y[i])
    }
    ")
  } else {
    likelihood <- paste0(likelihood, "
    tau_Y ~ dgamma(0.1, 0.1)

    for (i in 1:N) {
      mu_Y[i] <- beta_0 + beta_Z * Z[i] + sum(C[i, 1:p_outcome] * beta_C[1:p_outcome]) + beta_U * U[i]
      Y[i] ~ dnorm(mu_Y[i], tau_Y)
    }
    ")
  }

  unmeasured_confounder <- "
  # Unmeasured Confounder parameters

  for (u in 1:p_unmeasured_confounder) {
    gamma_C[u] ~ dunif(-2, 2)
  }

  for (i in 1:N) {
    logit(p_U[i]) <- sum(C[i, 1:p_unmeasured_confounder] * gamma_C[1:p_unmeasured_confounder])
    U[i] ~ dbern(p_U[i])
  }
  "

  treatment_model <- "
  # Treatment model parameters

  alpha_0 ~ dunif(-2, 2)

  for (z in 1:p_treatment) {
    alpha_C[z] ~ dunif(-2, 2)
  }

  alpha_U ~ dunif(-2, 2)

  for (i in 1:N) {
    logit(p_Z[i]) <- alpha_0 + sum(C[i, 1:p_treatment] * alpha_C[1:p_treatment]) + alpha_U * U[i]
    Z[i] ~ dbern(p_Z[i])
  }
  "

  return(paste0(
    "model{\n",
    likelihood,
    unmeasured_confounder,
    treatment_model,
    "\n}"
  ))
}
