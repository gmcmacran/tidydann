# See https://www.tidymodels.org/learn/develop/models/

#' @title Discriminant Adaptive Nearest Neighbor Classification
#' @param mode A single character string for the type of model. The only
#' possible value for this model is "classification".
#' @param neighbors The number of data points used for final classification.
#' @param neighborhood The number of data points used to calculate between and
#' within class covariance.
#' @param matrix_diagonal Diagonal elements of a diagonal matrix. 1 is the
#' identity
#' matrix.
#' @return  An S3 class of type tidy_dann.
#' @details
#' Discriminant Adaptive Nearest Neighbor (dann) is a variation of k nearest
#' neighbors where the shape of the neighborhood is data driven. The
#' neighborhood is elongated along class boundaries and shrunk in the orthogonal
#' direction.
#'
#' The only engine for this model is dann.
#' @examples
#'
#' library(rsample)
#' library(parsnip)
#' library(tidydann)
#'
#' data("two_class_dat", package = "modeldata")
#' set.seed(1)
#' example_split <- initial_split(two_class_dat, prop = 0.99)
#' example_train <- training(example_split)
#' example_test <- testing(example_split)
#'
#' model <- tidy_dann(neighbors = 2) |>
#'   set_engine("dann") |>
#'   fit(formula = Class ~ A + B, data = example_train)
#'
#' model |>
#'   predict(new_data = example_test)
#'
#' @export
tidy_dann <- function(mode = "classification", neighbors = NULL,
                      neighborhood = NULL, matrix_diagonal = NULL) {
  # Check for correct mode
  if (mode != "classification") {
    rlang::abort("`mode` should be 'classification'")
  }

  # Capture the arguments in quosures
  args <- list(
    neighbors = rlang::enquo(neighbors),
    neighborhood = rlang::enquo(neighborhood),
    matrix_diagonal = rlang::enquo(matrix_diagonal)
  )

  # Save some empty slots for future parts of the specification
  parsnip::new_model_spec(
    cls = "tidy_dann",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
}

#' @title Update method for tidy_dann
#' @param  object A model specification.
#' @param  parameters A 1-row tibble or named list with main parameters to
#' update. Use either parameters or the main arguments directly when updating.
#' If the main arguments are used, these will supersede the values in
#' parameters. Also, using engine arguments in this object will result in an
#' error.
#' @inheritParams tidy_dann
#' @param  fresh A logical for whether the arguments should be modified
#' in-place or replaced wholesale.
#' @param  ... Not used for update().
#' @return  An updated parsnip spec for tidy_dann model.
#' @export
update.tidy_dann <- function(object, parameters = NULL, neighbors = NULL,
                             neighborhood = NULL, matrix_diagonal = NULL,
                             fresh = FALSE,
                             ...) {
  args <- list(
    neighbors = rlang::enquo(neighbors),
    neighborhood = rlang::enquo(neighborhood),
    matrix_diagonal = rlang::enquo(matrix_diagonal)
  )

  parsnip::update_spec(
    object = object,
    parameters = parameters,
    args_enquo_list = args,
    fresh = fresh,
    cls = "tidy_dann",
    ...
  )
}
