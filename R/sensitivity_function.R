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
#' @param form Form of the sensitivity function (character: "constant" or
#' "linear")
#' @param c1 Value of the sensitivity function when z = 1 (numeric)
#' @param c0 Value of the sensitivity function when z = 0 (numeric)
#' @param s1 Slope of the sensitivity function when z = 1 (numeric)
#' @param s0 Slope of the sensitivity function when z = 0 (numeric)
#'
#' @return Sensitivity of treatment effect estimate to unmeasured confounding
#' (numeric)
#'
#' sf(z, e, form)
#' @export
sf <- function(z, e, form = "constant", c1 = 0, c0 = 0, s1 = 0, s0 = 0) {
  if (form == "constant") {
    return(ifelse(z == 1, c1, c0))
  } else if (form == "linear") {
    if (is.null(s1) || is.null(s0)) {
      stop("s1 and s0 must be provided when sf is 'linear'")
    }
    return(ifelse(z == 1, c1 + s1 * e, c0 + s0 * e))
  }
}
