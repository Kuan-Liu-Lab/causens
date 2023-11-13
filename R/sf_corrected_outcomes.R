#' Outcomes Corrected via Sensitivity Function
#'
#' This function provides corrected outcomes corrected using a sensitivity
#' function c(z, e).
#'
#' @param trt_model The treatment model object.
#' @param data A data frame containing the variables of interest.
#' @param exposure The name of the exposure variable.
#' @param outcome The name of the outcome variable.
#' @param confounders A vector of names of confounding variables.
#' @param sf The R sensitivity function for c(z, e).
#'
#' @return A vector corresponding with corrected outcomes.
#'
#' @examples
#' data <- data.frame(
#'   exposure = c(0, 1, 0, 1),
#'   outcome = c(0, 1, 0, 1),
#'   confounder1 = c(1, 0, 1, 0),
#'   confounder2 = c(0, 1, 0, 1)
#' )
#' corrected_outcomes(data, "exposure", "outcome", c(
#'   "confounder1",
#'   "confounder2"
#' ), 0.5)
#'
#' @export
corrected_outcomes <- function(trt_model, data, exposure, outcome, sf) {
  # TODO: allow exposure and outcome to be strings rather than vectors

  if (is.function(sf) == FALSE) {
    stop("sf must be a function.")
  }

  y_sf <- c()
  predicted_exposure <- predict(trt_model, data, type = "response")

  for (i in seq_along(exposure)) {
    c_value <- sf(exposure[i], predicted_exposure[i]) # not to confuse with c() function
    y_sf <- c(y_sf, (outcome[i] - predicted_exposure[i]) * c_value)
  }
  return(y_sf)
}
