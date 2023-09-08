# See https://www.tidymodels.org/learn/develop/models/

parsnip::set_new_model("tidy_dann")
parsnip::set_model_mode(model = "tidy_dann", mode = "classification")
parsnip::set_model_engine(
  model = "tidy_dann",
  mode = "classification",
  eng = "dann"
)
parsnip::set_dependency(model = "tidy_dann", eng = "dann", pkg = "dann")

parsnip::show_model_info("tidy_dann")

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

parsnip::show_model_info("tidy_dann")

tidy_dann <- function(mode = "classification",  neighbors = 5, neighborhood=max(floor(nrow(data)/5), 50), epsilon=1) {
    # Check for correct mode
    if (mode  != "classification") {
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

parsnip::show_model_info("tidy_dann")

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
    args =
      list(
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
    args =
      list(
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

parsnip::show_model_info("tidy_dann")

tidy_dann(neighbors = 2) |>
  parsnip::translate(engine = "dann")

data("two_class_dat", package = "modeldata")
set.seed(4622)
example_split <- rsample::initial_split(two_class_dat, prop = 0.99)
example_train <- rsample::training(example_split)
example_test  <-  rsample::testing(example_split)

dann_spec <- tidy_dann(neighbors = 2) |>
  parsnip::set_engine("dann")

dann_fit <- dann_spec |>
  parsnip::fit(formula = Class ~ ., data = example_train, engine = "dann")
dann_fit


stats::predict(dann_fit, new_data = example_test, type = "prob") |>
  dplyr::bind_cols(example_test |> dplyr::select(Class))


stats::predict(dann_fit, new_data = example_test, type = "class") |>
  dplyr::bind_cols(example_test |> dplyr::select(Class))

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

tunable.tidy_dann <- function(x, ...) {
  tibble::tibble(
    name = c("neighbors", "neighborhood", "epsilon"),
    call_info = list(list(pkg = "dials", fun = "neighbors"), list(pkg = NULL, fun = "neighborhood"), list(pkg = NULL, fun = "epsilon")),
    source = "model_spec",
    component = "tidy_dann",
    component_id = "main"
  )
}


dann_spec <-
  tidy_dann(neighbors = hardhat::tune(), neighborhood=tune(), epsilon=tune()) |>
  parsnip::set_engine("dann")


set.seed(452)
cv <- rsample::vfold_cv(example_train)
dan_tune_res <- dann_spec |>
  tune::tune_grid(Class ~ ., cv, grid = 4)
tune::show_best(dan_tune_res, metric = "roc_auc")
