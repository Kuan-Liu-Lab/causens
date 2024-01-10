#' @title Outcomes Corrected via Sensitivity Function
#'
#' @description This function provides corrected outcomes corrected using a sensitivity
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
#' @inheritParams sf
corrected_outcomes <- function(trt_model, data, exposure, outcome, form = "constant", ...) {
  # TODO: allow exposure and outcome to be strings rather than vectors

  args <- list(...)

  if (!all(exposure %in% c(0, 1))) {
    stop("Non-binary exposures are not yet supported in causens.")
  }

  if (is.function(sf) == FALSE) {
    stop("sf must be a function or specified to be one of 'constant', 'linear' or 'quadratic'.")
  }

  if (is.character(sf)) {
    if (sf == "constant") {
      if (!"c1" %in% names(args) || !"c0" %in% names(args)) {
        stop("c1 and c0 must be provided when sf is 'constant'")
      }
      c1 <- args$c1
      c0 <- args$c0

      if (length(c1) != 1 || length(c0) != 1) {
        stop("c1 and c0 must be numeric scalars.")
      }
    } else if (sf == "linear") {
      # c1, c0, s1, s2 all carry a value of NULL if unspecified
      c1 <- args$c1
      c0 <- args$c0

      s1 <- args$s1
      s0 <- args$s0
    } else {
      stop("Invalid sensitivity function.")
    }
  }

  y_corrected <- c()
  predicted_exposure <- predict(trt_model, data, type = "response")

  for (i in seq_along(exposure)) {
    c_value <- sf(exposure[i], predicted_exposure[i], form, c1, c0, s1, s0)
    y_corrected <- c(y_corrected, (outcome[i] - predicted_exposure[i]) * c_value)
  }
  return(y_corrected)
}
