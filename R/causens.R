#' @title Estimate Corrected Average Treatment Effect
#'
#' @description This function provides an estimate of the Average Treatment
#' Effect (ATE) using outcomes corrected for unmeasured confounding via a
#' sensitivity function.
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
#' @inheritParams corrected_outcomes
causens <- function(
  trt_model,
  data,
  exposure,
  outcome,
  sf = "constant",
  c1 = 0.5,
  c0 = 0.3
) {
  # Check if the necessary columns exist
  if (!all(c("treatment", "outcome") %in% names(data))) {
    stop(paste("Dataframe does not have the necessary columns:",
               "'treatment' and 'outcome'"))
  }

  y_corrected <- corrected_outcomes(
    trt_model,
    data,
    exposure,
    outcome,
    sf = "constant",
    c1 = 0.5,
    c0 = 0.3
  )

  # Calculate the Average Treatment Effect
  ate <- (
    mean(y_corrected[exposure == 1] / (sum(exposure == 1)))
    - mean(y_corrected[exposure == 0] / (sum(exposure == 0)))
  )

  return(ate)
}
