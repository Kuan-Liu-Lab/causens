test_that("plot function generates base R plot", {
  # Create test data
  data <- simulate_data(
    N = 1000, alpha_uz = 1, beta_uy = 1,
    seed = 123, treatment_effects = 1
  )

  trt_model <- Z ~ X.1 + X.2 + X.3

  plot_obj <- plot_causens(
    trt_model,
    data,
    "Y",
    c1_upper = 0.5,
    c1_lower = 0,
    r = 1,
    bootstrap = FALSE
  )

  plot_obj <- recordPlot() # returns an error if no plot is created
  dev.off()

  # Test if a plot object was created
  expect_true(is.list(plot_obj))
  expect_true(length(plot_obj) > 0)
})
