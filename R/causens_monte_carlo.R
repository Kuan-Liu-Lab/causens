#' @title Monte Carlo sensitivity analysis for causal effects
#'
#' @description This function performs a Monte Carlo sensitivity analysis for causal effects.
#'
#' @param exposure The name of the exposure variable in the data frame.
#' @param outcome The name of the outcome variable in the data frame.
#' @param confounders The name of the confounders in the data frame.
#' @param data A data frame containing the exposure, outcome, and confounder variables.
#' @param ... Additional arguments to be passed to the function.
#' @return The estimated causal effect.
#' @importFrom stats as.formula rnorm runif sd
#' @export
causens_monte_carlo <- function(exposure, outcome, confounders, data, ...) {
  # rather than taking a general model formula, the model formulation here
  # is more restrictive and has to be additive without interactions (for now)
  formula <- as.formula(paste(outcome, "~", exposure, "+", paste(confounders, collapse = "+")))

  if (all(data[[outcome]] %in% c(0, 1))) {
    naive_model <- glm(formula, data = data, family = binomial)
  } else {
    stop("Monte Carlo sensitivity analysis only supports binary outcomes, for now")
  }

  trt_effect_hat <- summary(naive_model)$coefficients["Z", "Estimate"]
  trt_effect_sd <- summary(naive_model)$coefficients["Z", "Std. Error"]

  trt_effect_corrected <- function(trt_effect) {
    # Due to linting, the following parameters are abbreviated as follows:
    # bU: beta_U
    # g0: gamma_0
    # gX: gamma_X
    bU <- runif(1, -2, -2)
    g0 <- runif(1, -5, 5)
    gX <- runif(1, -2, 2)

    return(trt_effect - log((1 + exp(bU + g0 + gX)) * (1 + exp(g0))) + log((1 + exp(bU + g0)) * (1 + exp(g0 + gX))))
  }

  args <- list(...)

  if ("nsamples" %in% names(args)) {
    nsamples <- args$nsamples
  } else {
    nsamples <- 20000
  }

  mc_trt_effects <- c()

  for (i in 1:nsamples) {
    mc_trt_effects <- c(
      mc_trt_effects,
      rnorm(1, trt_effect_corrected(trt_effect_hat), trt_effect_sd)
    )
  }

  causens_obj <- list()

  causens_obj <- list()
  class(causens_obj) <- "monte_carlo_causens"
  causens_obj$call <- formula
  causens_obj$estimated_ate <- mean(mc_trt_effects)
  causens_obj$std_error <- sd(mc_trt_effects)
  causens_obj$ci <- quantile(mc_trt_effects, c(0.025, 0.975))

  return(causens_obj)
}
