#' @title Causal Effect Estimation with Sensitivity Analysis
#'
#' @description This function provides an estimate of the Average Treatment
#' Effect (ATE) using outcomes corrected for unmeasured confounding via a
#' sensitivity function.
#'
#' Arguments in the ellipsis are 'keyword arguments' and are passed
#' to the sensitivity function `sf`.
#'
#' @param trt_model The treatment model object.
#' @param data A data frame containing the variables of interest.
#' @param exposure The name of the exposure variable.
#' @param outcome The name of the outcome variable.
#' @param method The method to use for sensitivity analysis. Currently, only
#' "Li" is supported.
#' @param ... Additional arguments to be passed to the sensitivity function.
#'
#' @return A point estimate of the corrected ATE.
#'
#' @export
causens <- function(trt_model, data, exposure, outcome, method, ...) {
  z <- data[[exposure]]
  y <- data[[outcome]]

  e <- predict(trt_model, type = "response")

  if (method == "Li") {
    c1 <- sf(z = 1, e = e, ...)
    c0 <- sf(z = 0, e = 1 - e, ...)
  } else {
    stop("Method not recognized or not implemented yet.")
  }

  # Calculate the Average Treatment Effect
  weights <- 1 / ifelse(z == 1, e, 1 - e)

  if (all(y %in% c(0, 1))) {
    Y_sf <- y * (abs(1 - z - e) + exp((-1)**(z == 1) * ifelse(z, c1, c0) * abs(z - e)))
  } else {
    Y_sf <- y + (-1)**(z == 1) * abs(z - e) * ifelse(z, c1, c0)
  }

  # Potential outcomes corrected w.r.t. sensitivity function
  Y1_sf <- sum((Y_sf * weights)[z == 1]) / sum(weights[z == 1])
  Y0_sf <- sum((Y_sf * weights)[z == 0]) / sum(weights[z == 0])

  estimated_ate <- Y1_sf - Y0_sf

  return(estimated_ate)
}
