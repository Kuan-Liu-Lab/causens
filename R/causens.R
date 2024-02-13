#' @title Estimate Corrected Average Treatment Effect
#'
#' @description This function provides an estimate of the Average Treatment Effect (ATE) using
#' outcomes corrected for unmeasured confounding via a sensitivity function.
#'
#' @param trt_model The treatment model object.
#' @param data A data frame containing the variables of interest.
#' @param exposure The name of the exposure variable.
#' @param outcome The name of the outcome variable.
#' @param confounders A vector of names of confounding variables.
#' @param sf The R sensitivity function for c(z, e).
#'
#' @return A point estimate of the corrected ATE.
#'
#' @export
causens <- function(trt_model, data, exposure, outcome, c1 = 0.5, c0 = 0.3, s1 = 0, s0 = 0) {
  z <- data[[exposure]]
  y <- data[[outcome]]

  e <- predict(trt_model, type = "response")

  # Calculate the Average Treatment Effect
  weights <- 1 / ifelse(z == 1, e, 1 - e)
  Y_sf <- y + (-1)**(z == 1) * abs(z - e) * ifelse(z, c1, c0)

  # Potential outcomes corrected w.r.t. sensitivity function
  Y1_sf <- sum((Y_sf * weights)[z == 1]) / sum(weights[z == 1])
  Y0_sf <- sum((Y_sf * weights)[z == 0]) / sum(weights[z == 0])

  estimated_ate <- Y1_sf - Y0_sf

  return(estimated_ate)
}
