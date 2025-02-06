#' @title Bayesian Estimation of ATE Subject to Unmeasured Confounding
#'
#' @description This function provides an estimate of the Average Treatment
#' Effect (ATE) using Bayesian modelling.
#'
#' @param trt_model A model formula specifying the treatment model.
#' @param outcome The name of the outcome variable.
#' @param data A data frame containing the exposure, outcome, and confounder
#' variables.
#' @param bootstrap A logical indicating whether to perform bootstrap estimation
#' of the 95\% confidence interval.
#' @param B If the bootstrap argument is TRUE, the number of bootstrap samples
#' to generate.
#' @param ... Additional arguments to be passed to the sensitivity function.
#' @importFrom stats predict
#'
#' @return A point estimate of the corrected ATE.
#' @export
causens_sf <- function(trt_model, outcome, data, bootstrap = FALSE,
                       B = 1000, ...) {
  processed_info <- process_model_formula(trt_model, data)
  y <- data[[outcome]]
  z <- data[[processed_info$response_var_name]]

  e <- predict(processed_info$fitted_model, type = "response")

  c1 <- sf(z = 1, e = e, ...) # c1, c0, s1, s0 may be passed as kwargs
  c0 <- sf(z = 0, e = 1 - e, ...)

  # Calculate the Average Treatment Effect
  weights <- 1 / ifelse(z == 1, e, 1 - e)

  Y_sf <- y + (-1)**(z == 1) * abs(z - e) * ifelse(z, c1, c0)

  # Potential outcomes corrected w.r.t. sensitivity function
  Y1_sf <- sum((Y_sf * weights)[z == 1]) / sum(weights[z == 1])
  Y0_sf <- sum((Y_sf * weights)[z == 0]) / sum(weights[z == 0])

  causens_obj <- list()
  class(causens_obj) <- "causens_sf"
  causens_obj$call <- formula(trt_model)
  causens_obj$estimated_ate <- Y1_sf - Y0_sf

  if (!bootstrap) {
    return(causens_obj)
  }

  # Implement bootstrap estimation of 95% confidence interval

  # Number of bootstrap samples

  ate_bs <- numeric(B)
  set.seed(123) # for bootstrap replications

  for (b in 1:B) {
    data_b <- data[sample(nrow(data), replace = TRUE), ]
    y_b <- data_b[[outcome]]
    z_b <- data_b[[processed_info$response_var_name]]

    e_b <- predict(processed_info$fitted_model,
      newdata = data_b,
      type = "response"
    )

    c1_b <- sf(z = 1, e = e_b, ...)
    c0_b <- sf(z = 0, e = 1 - e_b, ...)

    weights_b <- 1 / ifelse(z_b == 1, e_b, 1 - e_b)

    Y_sf_b <- y_b + (-1)**(z_b == 1) * abs(z_b - e_b) * ifelse(z_b, c1_b, c0_b)

    Y1_sf_b <- sum((Y_sf_b * weights_b)[z_b == 1]) / sum(weights_b[z_b == 1])
    Y0_sf_b <- sum((Y_sf_b * weights_b)[z_b == 0]) / sum(weights_b[z_b == 0])

    ate_bs[b] <- Y1_sf_b - Y0_sf_b
  }

  causens_obj$ci <- quantile(ate_bs, c(0.025, 0.975))
  causens_obj$std_error <- sd(ate_bs)

  return(causens_obj)
}
