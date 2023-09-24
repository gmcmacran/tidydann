# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start

#' @keywords internal
make_tidy_dann_dann <- function() {
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

# nocov end
