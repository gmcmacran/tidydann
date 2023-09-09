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

#' @title Neighborhood size
#'
#' @description Number of data points used to calculate shape of the neighborhood.
#' @param range	A two-element vector holding the defaults for the smallest and largest possible values, respectively.
#' If a transformation is specified, these values should be in the transformed units.
#' @param trans	A trans object from the scales package, such as scales::log10_trans() or scales::reciprocal_trans().
#' If not provided, the default is used which matches the units used in range. If no transformation, NULL.
#' @details
#' A static range is used but a broader range should be used if the data set
#' is large.
#' @examples
#' neighborhood()
#' @export
neighborhood <- function(range = c(1L, 50L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(neighborhood = "# Neighborhood"),
    finalize = NULL
  )
}

#' @title Softening
#'
#' @inheritParams neighborhood
#' @details
#' Softening parameter. Usually has the least affect on performance.
#' @examples
#' epsilon()
#' @export
epsilon <- function(range = c(0, 3), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(FALSE, TRUE),
    trans = trans,
    label = c(epsilon = "# Epsilon"),
    finalize = NULL
  )
}

#' @title Declare tunable parameters
#'
#' @description Returns information on potential hyper-parameters that can be optimized.
#'
#' @param x A model specification of type tidy_dann.
#' specification.
#' @param ... Other arguments passed to methods.
#' @return A tibble with a column for the parameter name, information on the default method for
#' generating a corresponding parameter object, the source of the parameter (e.g. "recipe", etc.),
#'  and the component within the source.
#' @export
tunable.tidy_dann <- function(x, ...) {
  tibble::tibble(
    name = c("neighbors", "neighborhood", "epsilon"),
    call_info = list(list(pkg = "dials", fun = "neighbors"), list(pkg = "tidydann", fun = "neighborhood"), list(pkg = "tidydann", fun = "epsilon")),
    source = "model_spec",
    component = "tidy_dann",
    component_id = "main"
  )
}
