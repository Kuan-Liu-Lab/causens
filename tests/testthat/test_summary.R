data_sf <- simulate_data(
  N = 10000, seed = 123, alpha_uz = 1,
  beta_uy = 1, treatment_effects = 1
)

result_sf <- causens(Z ~ X.1 + X.2 + X.3, "Y",
  data = data_sf, method = "sf",
  c1 = 0.25, c0 = 0.25, bootstrap = TRUE
)

summary_table <- capture_output(summary(result_sf))

test_that("summary.causens_sf produces correct output", {
  expect_equal(summary_table, paste(
    "Treatment Model:",
    "Z ~ X.1 + X.2 + X.3 ",
    "",
    "Estimate     Std.Error    95% C.I.             ",
    "1.01         0.0285       (0.947, 1.06)                  ",
    sep = "\n"
  ))
})
