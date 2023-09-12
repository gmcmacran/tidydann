#' @keywords internal
make_tidy_sub_dann_model <- function() {
  parsnip::set_new_model("tidy_sub_dann")

  parsnip::set_model_mode(model = "tidy_sub_dann", mode = "classification")
  parsnip::set_model_engine(
    model = "tidy_sub_dann",
    mode = "classification",
    eng = "sub_dann"
  )
  parsnip::set_dependency(model = "tidy_sub_dann", eng = "sub_dann", pkg = "dann", mode = "classification")
  parsnip::set_dependency(model = "tidy_sub_dann", eng = "sub_dann", pkg = "tidydann", mode = "classification")

  parsnip::set_model_arg(
    model = "tidy_sub_dann",
    eng = "sub_dann",
    parsnip = "neighbors",
    original = "k",
    func = list(pkg = "dann", fun = "sub_dann"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tidy_sub_dann",
    eng = "sub_dann",
    parsnip = "neighborhood",
    original = "neighborhood_size",
    func = list(pkg = "dann", fun = "sub_dann"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tidy_sub_dann",
    eng = "sub_dann",
    parsnip = "epsilon",
    original = "epsilon",
    func = list(pkg = "dann", fun = "sub_dann"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tidy_sub_dann",
    eng = "sub_dann",
    parsnip = "weighted",
    original = "weighted",
    func = list(pkg = "dann", fun = "sub_dann"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tidy_sub_dann",
    eng = "sub_dann",
    parsnip = "sphere",
    original = "sphere",
    func = list(pkg = "dann", fun = "sub_dann"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tidy_sub_dann",
    eng = "sub_dann",
    parsnip = "num_comp",
    original = "numDim",
    func = list(pkg = "dann", fun = "sub_dann"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "tidy_sub_dann",
    eng = "sub_dann",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "dann", fun = "sub_dann"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "tidy_sub_dann",
    eng = "sub_dann",
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
      args =
        list(
          object = quote(object$fit),
          new_data = quote(new_data),
          type = "class"
        )
    )

  parsnip::set_pred(
    model = "tidy_sub_dann",
    eng = "sub_dann",
    mode = "classification",
    type = "class",
    value = class_info
  )

  class_info <-
    list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          new_data = quote(new_data),
          type = "prob"
        )
    )

  parsnip::set_pred(
    model = "tidy_sub_dann",
    eng = "sub_dann",
    mode = "classification",
    type = "prob",
    value = class_info
  )
}

# See https://www.tidymodels.org/learn/develop/models/

#' @title Discriminant Adaptive Nearest Neighbor With Subspace Reduction
#' @inheritParams tidy_dann
#' @param weighted weighted argument to ncoord. See [fpc::ncoord()] for details.
#' @param sphere One of "mcd", "mve", "classical", or "none" See [fpc::ncoord()] for details.
#' @param num_comp Dimension of subspace used by dann. See [fpc::ncoord()] for details.
#' @return  An S3 class of type tidy_sub_dann.
#' @details
#' dann's performance suffers when noise variables are included in the model. Simulations show sub_dann
#' will generally be more performant in this scenario.
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
#' model <- tidy_sub_dann(neighbors = 2) |>
#'   set_engine("sub_dann") |>
#'   fit(formula = Class ~ A + B, data = example_train)
#'
#' model |>
#'   predict(new_data = example_test)
#'
#' @export
tidy_sub_dann <- function(mode = "classification", neighbors = NULL, neighborhood = NULL,
                          epsilon = NULL, weighted = NULL, sphere = NULL, num_comp = NULL) {
  # Check for correct mode
  if (mode != "classification") {
    rlang::abort("`mode` should be 'classification'")
  }

  # Capture the arguments in quosures
  args <- list(
    neighbors = rlang::enquo(neighbors), neighborhood = rlang::enquo(neighborhood),
    epsilon = rlang::enquo(epsilon), weighted = rlang::enquo(weighted),
    sphere = rlang::enquo(sphere), num_comp = rlang::enquo(num_comp)
  )

  # Save some empty slots for future parts of the specification
  parsnip::new_model_spec(
    cls = "tidy_sub_dann",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
}
