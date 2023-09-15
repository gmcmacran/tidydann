# See https://www.tidymodels.org/learn/develop/models/

#' @title Discriminant Adaptive Nearest Neighbor With Subspace Reduction
#' @inheritParams tidy_dann
#' @param weighted weighted argument to ncoord. See [fpc::ncoord()] for details.
#' @param sphere One of "mcd", "mve", "classical", or "none" See [fpc::ncoord()]
#' for details.
#' @param num_comp Dimension of subspace used by dann. See [fpc::ncoord()] for
#' details.
#' @return  An S3 class of type tidy_sub_dann.
#' @details
#' dann's performance suffers when noise variables are included in the model.
#' Simulations show sub_dann will generally be more performant in this scenario.
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
tidy_sub_dann <- function(mode = "classification", neighbors = NULL,
                          neighborhood = NULL, epsilon = NULL, weighted = NULL,
                          sphere = NULL, num_comp = NULL) {
  # Check for correct mode
  if (mode != "classification") {
    rlang::abort("`mode` should be 'classification'")
  }

  # Capture the arguments in quosures
  args <- list(
    neighbors = rlang::enquo(neighbors),
    neighborhood = rlang::enquo(neighborhood),
    epsilon = rlang::enquo(epsilon),
    weighted = rlang::enquo(weighted),
    sphere = rlang::enquo(sphere),
    num_comp = rlang::enquo(num_comp)
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

#' @title Update method for tidy_sub_dann
#' @title Update method for tidy_dann
#' @param  object A model specification.
#' @param  parameters A 1-row tibble or named list with main parameters to
#' update. Use either parameters or the main arguments directly when updating.
#' If the main arguments are used, these will supersede the values in
#' parameters. Also, using engine arguments in this object will result in an
#' error.
#' @inheritParams tidy_sub_dann
#' @param  fresh A logical for whether the arguments should be modified
#' in-place or replaced wholesale.
#' @param  ... Not used for update().
#' @export
#' @export
update.tidy_sub_dann <- function(object, parameters = NULL, neighbors = NULL,
                                 neighborhood = NULL, epsilon = NULL,
                                 weighted = NULL, sphere = NULL,
                                 num_comp = NULL, fresh = FALSE,
                                 ...) {
  args <- list(
    neighbors = rlang::enquo(neighbors),
    neighborhood = rlang::enquo(neighborhood),
    epsilon = rlang::enquo(epsilon),
    weighted = rlang::enquo(weighted),
    sphere = rlang::enquo(sphere),
    num_comp = rlang::enquo(num_comp)
  )

  parsnip::update_spec(
    object = object,
    parameters = parameters,
    args_enquo_list = args,
    fresh = fresh,
    cls = "tidy_sub_dann",
    ...
  )
}
