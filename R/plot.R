#' @title Plot ATE with respect to sensitivity function value
#' when it is constant, i.e. c(1, e) = c1 and c(0, e) = c0.
#' @description This function plots 1) the ATE as a function of the sensitivity
#' function value when it is constant and 2) its associate 95% confidence
#' interval obtained via bootstrapping, if desired. The latter process can be
#' take a few seconds to minutes.
#'
#' @param trt_model The treatment model object as a formula or fitted glm.
#' @param data A data frame containing the variables of interest.
#' @param outcome The name of the outcome variable.
#' @param c1_upper The upper bound for the sensitivity function value.
#' @param c1_lower The lower bound for the sensitivity function value.
#' @param r The ratio between c1 and c0.
#' @param by The increment for the sensitivity function value.
#' @importFrom stats quantile sd update
#' @importFrom graphics plot lines legend
#'
#' @return A plot of the ATE as a function of c1 values.
#'
#' @export
plot_causens <- function(trt_model, data, outcome, c1_upper = 0.5,
                         c1_lower = 0, r = 1, by = 0.01) {
  adjusted_ates <- c()
  lower_ci <- c()
  upper_ci <- c()

  c1_values <- seq(c1_lower, c1_upper, by = by)

  for (c1 in c1_values) {
    causens_obj_c1 <- causens(
      trt_model = trt_model,
      data = data,
      outcome = outcome,
      method = "sf",
      c1 = c1,
      c0 = r * c1,
      bootstrap = TRUE
    )

    adjusted_ates <- c(adjusted_ates, causens_obj_c1$estimated_ate)
    lower_ci <- c(lower_ci, causens_obj_c1$ci[1])
    upper_ci <- c(upper_ci, causens_obj_c1$ci[2])
  }

  plot_df <- data.frame(c1 = c1_values, ate = adjusted_ates)

  plot(plot_df$c1, plot_df$ate,
    type = "l", pch = 20, col = "black",
    xlab = "Sensitivity Function Value (c1)",
    ylab = "Adjusted Treatment Effect (ATE)",
    main = "ATE vs. Sensitivity Function Value (c1)"
  )

  lines(plot_df$c1, lower_ci, col = "black", lty = 2)
  lines(plot_df$c1, upper_ci, col = "black", lty = 2)

  legend("topright",
    legend = c("ATE", "95% CI"),
    col = c("black", "black"), lty = c(1, 2), cex = 0.8
  )
}
