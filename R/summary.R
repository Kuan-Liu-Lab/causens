#' Summarize the results of a causal sensitivity analysis via
#' sensitivity function.
#'
#' @param object An object of class \code{causens_sf}.
#' @param ... Additional arguments to be passed to \code{summary}.
#'
#' @return A summary of the results of the causal sensitivity analysis.
#' @export
summary.causens_sf <- function(object, ...) {
  # 1) Showing treatment model call
  cat("Treatment Model:\n")
  cat(deparse(object$call), "\n\n")

  # 2) Printing table (estimate, std.error, 95% CI)
  header <- sprintf(
    paste0(
      "%-", 12, "s ",
      "%-", 12, "s ",
      "%-", 20, "s"
    ),
    "Estimate", "Std.Error", "95% C.I."
  )

  ci_lower <- format(object$ci[1], digits = 3)
  ci_upper <- format(object$ci[2], digits = 3)

  data_line <- sprintf(
    paste0(
      "%-", 12, "s ",
      "%-", 12, "s ",
      "%-", 30, "s"
    ),
    format(object$estimated_ate, digits = 3),
    format(object$std_error, digits = 3),
    paste0("(", ci_lower, ", ", ci_upper, ")")
  )

  cat(header, "\n")
  cat(data_line, "\n")
}

#' Summarize the results of a causal sensitivity analysis via
#' Bayesian modelling of an unmeasured confounder.
#'
#' @param object An object of class \code{bayesian_causens}.
#' @param ... Additional arguments to be passed to \code{summary}.
#'
#' @export
summary.bayesian_causens <- function(object, ...) {
  # 1) Showing treatment model call
  cat("Treatment Model:\n")
  cat(deparse(object$call), "\n\n")

  # 2) Printing table (estimate, std.error, 95% CI)
  header <- sprintf(
    paste0(
      "%-", 12, "s ",
      "%-", 12, "s ",
      "%-", 25, "s"
    ),
    "Estimate", "Std.Error", "95% Credible Interval"
  )

  ci_lower <- format(object$ci[1], digits = 3)
  ci_upper <- format(object$ci[2], digits = 3)

  data_line <- sprintf(
    paste0(
      "%-", 12, "s ",
      "%-", 12, "s ",
      "%-", 30, "s"
    ),
    format(object$estimated_ate, digits = 3),
    format(object$std_error, digits = 3),
    paste0("(", ci_lower, ", ", ci_upper, ")")
  )

  cat(header, "\n")
  cat(data_line, "\n")
}

#' Summarize the results of a causal sensitivity analysis via the
#' Monte Carlo method.
#'
#' @param object An object of class \code{causens_monte_carlo}.
#' @param ... Additional arguments to be passed to \code{summary}.
#'
#' @export
summary.monte_carlo_causens <- function(object, ...) {
  return(summary.causens_sf(object, ...))
}

# Helper function for displaying small numbers
format_number <- function(x, digits = 3, threshold = 1e-3) {
  if (abs(x) < threshold) {
    return(format(x, scientific = TRUE, digits = digits))
  } else {
    return(format(round(x, digits), nsmall = digits))
  }
}
