#' Causal sensitivity analysis
#'
#' TODO: Write description
#'
#' @param out.model The outcome model object on which we wish to perform a sensitivity analysis
#' @param trt.model The treatment model object
#' @param data A data frame containing all variables used in the outcome and treatment models
#' @param exposure The name of the exposure variable
#' @param outcome The name of the outcome variable
#' @param sf The sensitivity function c(z, e)
#' @return NA
#' 
#' @export
#' @inheritParams sf
causens <- function(out.model, trt.model, data, exposure, outcome, sf="increasing"){
  y.sf <- corrected_outcomes(trt.model, data, exposure, outcome, sf)
  out.model.sf <- glm(y.sf ~ exposure, data = data, family = binomial(link = "logit"))

  return (out.model.sf)
}
