#' @title Bayesian Estimation of ATE Subject to Unmeasured Confounding
#'
#' @description This function provides an estimate of the Average Treatment
#' Effect (ATE) using Bayesian modelling.
#'
#' @param fitted_model The treatment model object as a glm.
#' @param exposure The name of the exposure variable.
#' @param outcome The name of the outcome variable.
#' @param data A data frame containing the exposure, outcome, and confounder variables.
#' @param ... Additional arguments to be passed to the sensitivity function.
#'
#' @return A point estimate of the corrected ATE.
#' @export
causens_sf <- function(fitted_model, exposure, outcome, data, ...) {
  y <- data[[outcome]]
  z <- data[[exposure]]

  e <- predict(fitted_model, type = "response")

  c1 <- sf(z = 1, e = e, ...)
  c0 <- sf(z = 0, e = 1 - e, ...)

  # Calculate the Average Treatment Effect
  weights <- 1 / ifelse(z == 1, e, 1 - e)

  Y_sf <- y + (-1)**(z == 1) * abs(z - e) * ifelse(z, c1, c0)

  # Potential outcomes corrected w.r.t. sensitivity function
  Y1_sf <- sum((Y_sf * weights)[z == 1]) / sum(weights[z == 1])
  Y0_sf <- sum((Y_sf * weights)[z == 0]) / sum(weights[z == 0])

  estimated_ate <- Y1_sf - Y0_sf

  return(estimated_ate)
}
