#' @keywords internal
make_tidy_dann_model <- function() {
  parsnip::set_new_model("tidy_dann")

  parsnip::set_model_mode(model = "tidy_dann", mode = "classification")
  parsnip::set_model_engine(
    model = "tidy_dann",
    mode = "classification",
    eng = "dann"
  )
  parsnip::set_dependency(
    model = "tidy_dann", eng = "dann", pkg = "dann",
    mode = "classification"
  )
  parsnip::set_dependency(
    model = "tidy_dann", eng = "dann", pkg = "tidydann",
    mode = "classification"
  )

  parsnip::set_model_arg(
    model = "tidy_dann",
    eng = "dann",
    parsnip = "neighbors",
    original = "k",
    func = list(pkg = "dann", fun = "dann"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tidy_dann",
    eng = "dann",
    parsnip = "neighborhood",
    original = "neighborhood_size",
    func = list(pkg = "dann", fun = "dann"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tidy_dann",
    eng = "dann",
    parsnip = "epsilon",
    original = "epsilon",
    func = list(pkg = "dann", fun = "dann"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "tidy_dann",
    eng = "dann",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "dann", fun = "dann"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "tidy_dann",
    eng = "dann",
    mode = "classification",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  class_info <-
    list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "class"
      )
    )

  parsnip::set_pred(
    model = "tidy_dann",
    eng = "dann",
    mode = "classification",
    type = "class",
    value = class_info
  )

  class_info <-
    list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "prob"
      )
    )

  parsnip::set_pred(
    model = "tidy_dann",
    eng = "dann",
    mode = "classification",
    type = "prob",
    value = class_info
  )
}

# See https://www.tidymodels.org/learn/develop/models/

#' @title Discriminant Adaptive Nearest Neighbor Classification
#' @param mode A single character string for the type of model. The only
#' possible value for this model is "classification".
#' @param neighbors The number of data points used for final classification.
#' @param neighborhood The number of data points used to calculate between and
#' within class covariance.
#' @param epsilon Diagonal elements of a diagonal matrix. 1 is the identity
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
                      neighborhood = NULL, epsilon = NULL) {
  # Check for correct mode
  if (mode != "classification") {
    rlang::abort("`mode` should be 'classification'")
  }

  # Capture the arguments in quosures
  args <- list(
    neighbors = rlang::enquo(neighbors),
    neighborhood = rlang::enquo(neighborhood),
    epsilon = rlang::enquo(epsilon)
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
