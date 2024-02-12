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
  z <- handle_argument(exposure, data)
  y <- handle_argument(outcome, data)

  args <- list(...)

  if (is.function(form)) {
    stop("form must be a function or specified to be one of 'constant', 'linear' or 'quadratic'.")
  }

  if (is.character(form)) {
    if (form == "constant") {
      if (!"c1" %in% names(args) || !"c0" %in% names(args)) {
        stop("c1 and c0 must be provided when form is 'constant'")
      }
      c1 <- args$c1
      c0 <- args$c0

      if (length(c1) != 1 || length(c0) != 1) {
        stop("c1 and c0 must be numeric scalars.")
      }

      s1 <- 0
      s0 <- 0

    } else if (form == "linear") {
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
  e <- predict(trt_model, data, type = "response")

  c_values <- mapply(
    sf,
    z,
    e,
    MoreArgs = list(form = form, c1 = c1, c0 = c0, s1 = s1, s0 = s0)
  )
  if (all(y %in% c(0, 1))) {
    # binary outcome
    y_corrected <- y * (abs(1 - z - e) + exp(- sf(z, e)) * abs(z - e))
  } else {
    y_corrected <- (y - e) * c_values
  }

  return(y_corrected)
}


#' @title Handle Argument
#' @description This function handles arguments passed to other functions.
#' @param arg The argument to be handled.
#' @param data The data frame containing the argument.
#' @return The corresponding vector in the data frame or the vector itself if
#' it was originally passed into the function.
#' @export
handle_argument <- function(arg, data) {
  arg_name <- deparse(substitute(arg))

  if (arg_name %in% names(data)) {
    return(data[[arg_name]])
  } else if (!is.vector(arg)) {
    stop(paste(arg_name, "must be a vector or the name of a column in data."))
  }

  return(arg)
}
