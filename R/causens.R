#' Causal sensitivity analysis
#'
#' TODO: Write description
#'
#' @param out_model The outcome model object on which we wish to perform a
#' sensitivity analysis
#' @param trt_model The treatment model object
#' @param data A data frame containing all variables used in the outcome and
#' treatment models
#' @param exposure The name of the exposure variable
#' @param outcome The name of the outcome variable
#' @param sf The sensitivity function c(z, e)
#' @return NA
#'
#' @export
#' @inheritParams corrected_outcomes
causens <- function(
    out_model,
    trt_model,
    data,
    exposure,
    outcome,
    sf = "increasing") {
  out_model_sf <- glm(
    corrected_outcomes(trt_model, data, exposure, outcome, sf) ~ exposure,
    data = data,
    family = binomial(link = "logit")
  )

  return(out_model_sf)
}
