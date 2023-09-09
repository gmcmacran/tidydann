# See https://www.tidymodels.org/learn/develop/models/

#' @title Discriminant Adaptive Nearest Neighbor Classification
#' @param mode A single character string for the type of model. The only possible value for this model is "classification".
#' @param neighbors The number of data points used for final classification.
#' @param neighborhood The number of data points used to calculate between and within class covariance.
#' @param epsilon Diagonal elements of a diagonal matrix. 1 is the identity matrix.
#' @return  An S3 class of type tidy_dann.
#' @details
#' Discriminant Adaptive Nearest Neighbor (dann) is a variation of k nearest neighbors where the shape of the neighborhood
#' is data driven. The neighborhood is elongated along class boundaries and shrunk in the orthogonal direction.
#'
#' The only engine for this model is dann.
#' @examples
#'
#' library(tidymodels)
#' library(tidydann)
#'
#' data("two_class_dat", package = "modeldata")
#' set.seed(4622)
#' example_split <- rsample::initial_split(two_class_dat, prop = 0.99)
#' example_train <- rsample::training(example_split)
#' example_test <- rsample::testing(example_split)
#'
#' tidy_dann(neighbors = 2) |>
#'   set_engine("dann") |>
#'   fit(formula = Class ~ A + B, data = example_train, engine = "dann")
#'
#' @export
tidy_dann <- function(mode = "classification", neighbors = 5, neighborhood = max(floor(nrow(data) / 5), 50), epsilon = 1) {
  # Check for correct mode
  if (mode != "classification") {
    rlang::abort("`mode` should be 'classification'")
  }

  # Capture the arguments in quosures
  args <- list(neighbors = rlang::enquo(neighbors), neighborhood = rlang::enquo(neighborhood), epsilon = rlang::enquo(epsilon))

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
