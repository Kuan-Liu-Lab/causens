args <- list(
  N = 500,
  alpha_uz = 0.2,
  beta_uy = 0.5,
  treatment_effects = 1,
  seed = 123
)

parameters <- list(
  list(y_type = "cont", u_type = "binary"),
  list(y_type = "cont", u_type = "cont"),
  list(y_type = "binary", u_type = "cont"),
  list(y_type = "binary", u_type = "binary")
)

is_binary <- function(type) {
  if (type == "binary") {
    return(function(x) {
      all(x %in% c(0, 1))
    })
  } else if (type == "cont") {
    return(function(x) {
      !all(x %in% c(0, 1))
    })
  }
}

test_that("Simulated data works with causens", {
  for (test_params in parameters) {
    y_type <- test_params$y_type
    u_type <- test_params$u_type

    sim_data_function <- get(paste0("gData_U_", u_type, "_Y_", y_type))
    simulated_data <- do.call(sim_data_function, args)

    expect_true(is_binary(y_type)(simulated_data$Y))
    expect_true(is_binary(u_type)(simulated_data$U))
  }
})
