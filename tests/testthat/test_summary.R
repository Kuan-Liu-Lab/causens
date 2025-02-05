data_sf <- simulate_data(
  N = 10000, seed = 123, alpha_uz = 1,
  beta_uy = 1, treatment_effects = 1
)

result_sf <- causens_sf(Z ~ X.1 + X.2 + X.3, "Y",
  data = data_sf, c1 = 0.25, c0 = 0.25, bootstrap = TRUE
)

summary_table_sf <- capture_output(summary(result_sf))

test_that("summary.causens_sf produces correct output", {
  expect_equal(summary_table_sf, paste(
    "Model:",
    "Z ~ X.1 + X.2 + X.3 ",
    "",
    "Estimate     Std.Error    95% C.I.             ",
    "1.01         0.0285       (0.947, 1.06)                  ",
    sep = "\n"
  ))
})

data_mc <- simulate_data(
  N = 10000, seed = 123, y_type = "binary", alpha_uz = 1,
  beta_uy = 1, treatment_effects = 1
)


result_mc <- causens_monte_carlo("Y", "Z", c("X.1", "X.2", "X.3"),
  data = data_mc, method = "Monte Carlo"
)

summary_table_mc <- capture_output(summary(result_mc))

test_that("summary.monte_carlo_causens produces correct output", {
  expect_equal(summary_table_mc, paste(
    "Model:",
    "Y ~ Z + X.1 + X.2 + X.3 ",
    "",
    "Estimate     Std.Error    95% C.I.             ",
    "1.22         0.29         (0.573, 1.87)                  ",
    sep = "\n"
  ))
})
