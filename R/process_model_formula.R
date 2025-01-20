#' @title Process model formula
#'
#' @description This helper function takes in a model in its `formula` format
#' and returns a list comprising of various information, including the fitted
#' model as an R object.
#'
#' @param model The model object as a formula or fitted glm.
#' @param data A data frame used for fitting the model.
#' @importFrom stats binomial formula glm terms
#' @return A list containing the fitted model, model formula, response variable
#' name, and confounder names.
#' @export
process_model_formula <- function(model, data) {
  if (inherits(model, "formula")) {
    model_formula <- model
  } else if (inherits(model, "glm")) {
    model_formula <- formula(model)
    fitted_model <- model
  } else {
    stop("Treatment model must be a formula or a glm object.")
  }

  response_index <- attr(terms(model_formula), "response")
  response_var_name <- all.vars(model_formula)[[response_index]]

  if (inherits(model, "formula")) {
    if (all(data[[response_var_name]] %in% c(0, 1))) {
      fitted_model <- glm(model_formula, data = data, family = binomial)
    } else {
      fitted_model <- lm(model_formula, data = data)
    }
  }

  confounder_names <- attr(terms(model), "term.labels")

  if ("U" %in% confounder_names) {
    confounder_names <- confounder_names[confounder_names != "U"]
  }

  return(list(
    fitted_model = fitted_model,
    model_formula = model_formula,
    response_var_name = response_var_name,
    confounder_names = confounder_names
  ))
}
