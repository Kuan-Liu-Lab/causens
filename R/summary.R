#' Summarize the results of a causal sensitivity analysis via
#' sensitivity function.
#'
#' @param causens_obj An object of class \code{causens_sf}.
#'
#' @return A summary of the results of the causal sensitivity analysis.
#' @export
summary.causens_sf <- function(causens_obj) {
  # 1) Showing treatment model call
  cat("Treatment Model:\n")
  cat(deparse(causens_obj$call), "\n\n")

  # 2) Printing table (estimate, std.error, 95% CI)
  header <- sprintf(
    paste0(
      "%-", 12, "s ",
      "%-", 12, "s ",
      "%-", 20, "s"
    ),
    "Estimate", "Std.Error", "95% C.I."
  )

  ci_lower <- format(causens_obj$ci[1], digits = 3)
  ci_upper <- format(causens_obj$ci[2], digits = 3)

  data_line <- sprintf(
    paste0(
      "%-", 12, "s ",
      "%-", 12, "s ",
      "%-", 30, "s"
    ),
    format(causens_obj$estimated_ate, digits = 3),
    format(causens_obj$std_error, digits = 3),
    paste0("(", ci_lower, ", ", ci_upper, ")")
  )

  cat(header, "\n")
  cat(data_line, "\n")
}

#' Summarize the results of a causal sensitivity analysis via
#' Bayesian modelling of an unmeasured confounder.
#'
#' @param causens_obj An object of class \code{causens_bayesian}.
#'
#' @export
summary.bayesian_causens <- function(causens_obj) {
  # 1) Showing treatment model call
  cat("Treatment Model:\n")
  cat(deparse(causens_obj$call), "\n\n")

  # 2) Printing table (estimate, std.error, 95% CI)
  header <- sprintf(
    paste0(
      "%-", 12, "s ",
      "%-", 12, "s ",
      "%-", 25, "s"
    ),
    "Estimate", "Std.Error", "95% Credible Interval"
  )

  ci_lower <- format(causens_obj$ci[1], digits = 3)
  ci_upper <- format(causens_obj$ci[2], digits = 3)

  data_line <- sprintf(
    paste0(
      "%-", 12, "s ",
      "%-", 12, "s ",
      "%-", 30, "s"
    ),
    format(causens_obj$estimated_ate, digits = 3),
    format(causens_obj$std_error, digits = 3),
    paste0("(", ci_lower, ", ", ci_upper, ")")
  )

  cat(header, "\n")
  cat(data_line, "\n")
}

# Helper function for displaying small numbers
format_number <- function(x, digits = 3, threshold = 1e-3) {
  if (abs(x) < threshold) {
    return(format(x, scientific = TRUE, digits = digits))
  } else {
    return(format(round(x, digits), nsmall = digits))
  }
}
