#' @title Corrected ATE
#'
#' @description This function provides the corrected ATE using a sensitivity
#' function c(z, e).
#'
#' @param trt_model The treatment model object.
#' @param data A data frame containing the variables of interest.
#' @param exposure The name of the exposure variable.
#' @param outcome The name of the outcome variable.
#' @param sf The R sensitivity function for c(z, e).
#'
#' @return A vector corresponding with the corrected ATE.
#'
#' @examples
#' data <- data.frame(
#'   exposure = c(0, 1, 0, 1),
#'   outcome = c(0, 1, 0, 1),
#'   confounder1 = c(1, 0, 1, 0),
#'   confounder2 = c(0, 1, 0, 1)
#' )
#' corrected_ate(data, "exposure", "outcome", c(
#'   "confounder1",
#'   "confounder2"
#' ), 0.5)
corrected_ate <- function(trt_model, data, exposure, outcome, sf) {
  y_sf <- corrected_outcomes(trt_model, data, exposure, outcome, sf)
  return(mean(y_sf[exposure == 1]) - mean(y_sf[exposure == 0]))
}
