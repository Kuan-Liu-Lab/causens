#' @title Causal Effect Estimation with Sensitivity Analysis
#'
#' @description This function provides an estimate of the Average Treatment
#' Effect (ATE) using outcomes corrected for unmeasured confounding via a
#' sensitivity function.
#'
#' Arguments in the ellipsis are 'keyword arguments' and are passed
#' to the sensitivity function `sf`.
#'
#' @param trt_model The treatment model object as a formula or fitted glm.
#' @param outcome The name of the outcome variable.
#' @param method The method to use for sensitivity analysis. Currently, "Li" and
#' "Bayesian" are supported.
#' @param data A data frame containing the exposure, outcome, and confounder variables.
#' @param ... Additional arguments to be passed to the sensitivity function.
#' @importFrom stats binomial formula glm terms
#'
#' @return A point estimate of the corrected ATE.
#'
#' @export
causens <- function(trt_model, outcome, method, data, ...) {
  if (inherits(trt_model, "formula")) {
    fitted_model <- glm(trt_model, data = data, family = binomial)
    trt_formula <- trt_model
  } else if (inherits(trt_model, "glm")) {
    fitted_model <- trt_model
    trt_formula <- formula(trt_model)
  } else {
    stop("Treatment model must be a formula or a glm object.")
  }

  trt_index <- attr(terms(trt_formula), "response")
  trt_var_name <- all.vars(trt_formula)[[trt_index]]

  method <- tolower(method) # case-insensitive
  confounder_names <- attr(terms(trt_model), "term.labels")

  causens_obj <- list(data = data)

  if (method == "sf" || method == "li") {
    causens_obj <- causens_sf(fitted_model, trt_var_name, outcome, data, ...)
  } else if (method == "bayesian") {
    causens_obj <- bayesian_causens(trt_var_name, outcome, confounder_names, data, ...)
  } else if (method == "mc" || method == "monte carlo") {
    causens_obj <- causens_monte_carlo(trt_var_name, outcome, confounder_names, data, ...)
  } else {
    stop("Method not recognized or not implemented yet.")
  }

  causens_obj$call <- trt_formula

  return(causens_obj)
}
