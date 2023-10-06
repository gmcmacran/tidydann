# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start

#' @keywords internal
make_nearest_neighbor_adaptive_sub_dann <- function() {
  parsnip::set_model_engine(
    model = "nearest_neighbor_adaptive",
    mode = "classification",
    eng = "sub_dann"
  )
  parsnip::set_dependency(
    model = "nearest_neighbor_adaptive", eng = "sub_dann",
    pkg = "dann", mode = "classification"
  )
  parsnip::set_dependency(
    model = "nearest_neighbor_adaptive", eng = "sub_dann",
    pkg = "tidydann", mode = "classification"
  )

  parsnip::set_model_arg(
    model = "nearest_neighbor_adaptive",
    eng = "sub_dann",
    parsnip = "neighbors",
    original = "k",
    func = list(pkg = "dann", fun = "sub_dann"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "nearest_neighbor_adaptive",
    eng = "sub_dann",
    parsnip = "neighborhood",
    original = "neighborhood_size",
    func = list(pkg = "dann", fun = "sub_dann"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "nearest_neighbor_adaptive",
    eng = "sub_dann",
    parsnip = "matrix_diagonal",
    original = "epsilon",
    func = list(pkg = "dann", fun = "sub_dann"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "nearest_neighbor_adaptive",
    eng = "sub_dann",
    parsnip = "weighted",
    original = "weighted",
    func = list(pkg = "dann", fun = "sub_dann"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "nearest_neighbor_adaptive",
    eng = "sub_dann",
    parsnip = "sphere",
    original = "sphere",
    func = list(pkg = "dann", fun = "sub_dann"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "nearest_neighbor_adaptive",
    eng = "sub_dann",
    parsnip = "num_comp",
    original = "numDim",
    func = list(pkg = "dann", fun = "sub_dann"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "nearest_neighbor_adaptive",
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
    model = "nearest_neighbor_adaptive",
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
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "class"
      )
    )

  parsnip::set_pred(
    model = "nearest_neighbor_adaptive",
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
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "prob"
      )
    )

  parsnip::set_pred(
    model = "nearest_neighbor_adaptive",
    eng = "sub_dann",
    mode = "classification",
    type = "prob",
    value = class_info
  )
}

# nocov end
