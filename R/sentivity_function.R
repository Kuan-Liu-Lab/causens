#' Calculate sensitivity of treatment effect estimate to unmeasured confounding
#'
#' This function calculates the sensitivity of a treatment effect estimate to
#' unmeasured confounding, as described in Rosenbaum (2002). The sensitivity is
#' defined as the maximum strength of association between the unmeasured
#' confounder and the treatment assignment that would be needed to explain away
#' the observed treatment effect estimate. This function assumes that the
#' treatment assignment is binary and that the outcome is continuous.
#'
#' @param z Treatment assignment (binary: 0 or 1)
#' @param e Propensity score value (numeric)
#' @param form Form of the sensitivity function (character: "increasing" or
#' "decreasing")
#' @return Sensitivity of treatment effect estimate to unmeasured confounding
#' (numeric)
#'
#' sf(z, e, form)
#' @export
sf <- function(z, e, form = "increasing") {
  if (form == "increasing") {
    return(0.05 + 0.05 * e)
  } else if (form == "decreasing") {
    return(0.1 - 0.05 * e)
  } else {
    stop("Invalid sensitivity function form.")
  }
}
