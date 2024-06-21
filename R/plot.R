#' @title Plot ATE with respect to sensitivity function value
#' when it is constant, i.e. c(1, e) = c1 and c(0, e) = c0.
#' @description This function plots 1) the ATE as a function of the sensitivity
#' function value when it is constant and 2) its associate 95% confidence
#' interval obtained via bootstrapping, if desired. The latter process can be
#' take a few seconds to minutes.
#'
#' @param trt_model The treatment model object.
#' @param data A data frame containing the variables of interest.
#' @param exposure The name of the exposure variable.
#' @param outcome The name of the outcome variable.
#' @param method The method to use for sensitivity analysis (see `causens` function).
#' @param c1_upper The upper bound for the sensitivity function value.
#' @param c1_lower The lower bound for the sensitivity function value.
#' @param r The ratio between c1 and c0.
#' @param bootstrap Bootstrap or not current estimate process and plot
#' the estimated confidence interval.
#'
#' @return A plot of the ATE as a function of c1 values.
#'
#' @export
plot_causens <- function(trt_model, data, exposure, outcome, method,
                         c1_upper = 0.5, c1_lower = 0, r = 1, bootstrap = TRUE) {
  adjusted_ates <- c()

  for (c1 in seq(c1_lower, c1_upper, by = 0.01)) {
    ate <- causens(
      trt_model = trt_model,
      data = data,
      exposure = exposure,
      outcome = outcome,
      method = method,
      c1 = c1,
      c0 = r * c1
    )
    adjusted_ates <- c(adjusted_ates, ate)
  }

  plot_df <- data.frame(c1 = seq(c1_lower, c1_upper, by = 0.01), ate = adjusted_ates)

  plot(plot_df$c1, plot_df$ate, type = "l", pch = 20, col = "black",
       xlab = "Sensitivity Function Value (c1)",
       ylab = "Adjusted Treatment Effect (ATE)",
       main = "ATE vs. Sensitivity Function Value (c1)")

  if (bootstrap) {

    lower_ci <- c()
    upper_ci <- c()

    for (c1 in seq(c1_lower, c1_upper, by = 0.01)) {
      bs_ate <- c()

      for (bs_iter in 1:1000) {
        sampled_data <- data[sample(seq_len(nrow(data)), replace = TRUE), ]
        bs_ate <- c(
          bs_ate,
          causens(
            trt_model = glm(formula(trt_model), data = sampled_data,
                            family = binomial()),
            data = sampled_data,
            exposure = exposure,
            outcome = outcome,
            method = method,
            c1 = c1,
            c0 = r * c1
          )
        )
      }

      lower_ci <- c(lower_ci, quantile(bs_ate, 0.025))
      upper_ci <- c(upper_ci, quantile(bs_ate, 0.975))
    }

    lines(plot_df$c1, lower_ci, col = "black", lty = 2)
    lines(plot_df$c1, upper_ci, col = "black", lty = 2)

    legend("topright", legend = c("ATE", "95% CI"),
           col = c("black", "black"), lty = c(1, 2), cex = 0.8)
  }
}
