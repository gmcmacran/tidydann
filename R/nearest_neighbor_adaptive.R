#' @title Discriminant Adaptive Nearest Neighbor Classification
#' @description Defines a nearest neighbor model where the shape of the
#' neighborhood is learned from the data. This is a parsnip specification for
#' the models in the dann package.
#' @param mode A single character string for the type of model. The only
#' possible value for this model is "classification".
#' @param neighbors The number of nearest neighbors used to classify a point.
#' Identical to k in standard k nearest neighbors.
#' @param neighborhood The number of nearest neighbors used to estimate the
#' between and within class covariance matrices that shape the neighborhood.
#' @param matrix_diagonal Softening parameter. Scales the identity matrix added
#' to the between class covariance, which keeps the neighborhood from
#' collapsing onto the class boundary. 1 matches the publication.
#' @param weighted Should the between class covariance matrices be weighted?
#' FALSE matches the publication. Passed to [fpc::ncoord()]. Only used by the
#' sub_dann engine.
#' @param sphere Type of covariance matrix used to sphere the data. One of
#' "mcd", "mve", "classical", or "none". Passed to [fpc::ncoord()]. Only used
#' by the sub_dann engine.
#' @param num_comp Number of dimensions in the subspace dann is fit on. Only
#' used by the sub_dann engine.
#' @return An S3 class of type nearest_neighbor_adaptive.
#' @details
#' Discriminant Adaptive Nearest Neighbor (dann) is a variation of k nearest
#' neighbors where the shape of the neighborhood is data driven. The
#' neighborhood is elongated along class boundaries and shrunk in the orthogonal
#' direction.
#'
#' Two engines are available. The dann engine fits [dann::dann()] and uses
#' neighbors, neighborhood, and matrix_diagonal. The sub_dann engine fits
#' [dann::sub_dann()], which first projects the predictors onto a lower
#' dimensional subspace and then fits dann there. It uses every argument.
#' Setting weighted, sphere, or num_comp with the dann engine is an error.
#'
#' Prediction is parallelized. See [tidydann_set_threads()] to control how many
#' threads are used.
#'
#' @examples
#'
#' library(parsnip)
#' library(tidydann)
#'
#' data("two_class_dat", package = "modeldata")
#'
#' previous <- tidydann_set_threads(2)
#'
#' model <- nearest_neighbor_adaptive(neighbors = 2) |>
#'   set_engine("dann") |>
#'   fit(formula = Class ~ A + B, data = two_class_dat)
#'
#' model |>
#'   predict(new_data = two_class_dat)
#'
#' tidydann_set_threads(previous)
#'
#' @export
nearest_neighbor_adaptive <- function(mode = "classification", neighbors = NULL,
                                      neighborhood = NULL,
                                      matrix_diagonal = NULL, weighted = NULL,
                                      sphere = NULL, num_comp = NULL) {
  # Check for correct mode
  if (mode != "classification") {
    rlang::abort("`mode` should be 'classification'")
  }

  args <- list(
    neighbors = rlang::enquo(neighbors),
    neighborhood = rlang::enquo(neighborhood),
    matrix_diagonal = rlang::enquo(matrix_diagonal),
    weighted = rlang::enquo(weighted),
    sphere = rlang::enquo(sphere),
    num_comp = rlang::enquo(num_comp)
  )

  # Save some empty slots for future parts of the specification
  parsnip::new_model_spec(
    cls = "nearest_neighbor_adaptive",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
}

#' @title Update a model specification
#' @description If parameters of a model specification need to be modified,
#' update() can be used in lieu of recreating the object from scratch.
#' @method update nearest_neighbor_adaptive
#' @inheritParams nearest_neighbor_adaptive
#' @param object A model specification.
#' @param parameters A 1-row tibble or named list with main parameters to
#' update. Use either parameters or the main arguments directly when updating.
#' If the main arguments are used, these will supersede the values in
#' parameters. Also, using engine arguments in this object will result in an
#' error.
#' @param fresh A logical for whether the arguments should be modified in-place
#' or replaced wholesale.
#' @param ... Not used for update().
#' @return An updated S3 class of type nearest_neighbor_adaptive.
#' @examples
#'
#' library(parsnip)
#' library(tidydann)
#'
#' model <- nearest_neighbor_adaptive(neighbors = 2)
#'
#' model |> update(neighbors = 5)
#'
#' @export
update.nearest_neighbor_adaptive <- function(object, parameters = NULL,
                                             neighbors = NULL,
                                             neighborhood = NULL,
                                             matrix_diagonal = NULL,
                                             weighted = NULL, sphere = NULL,
                                             num_comp = NULL, fresh = FALSE,
                                             ...) {
  args <- list(
    neighbors = rlang::enquo(neighbors),
    neighborhood = rlang::enquo(neighborhood),
    matrix_diagonal = rlang::enquo(matrix_diagonal),
    weighted = rlang::enquo(weighted),
    sphere = rlang::enquo(sphere),
    num_comp = rlang::enquo(num_comp)
  )

  parsnip::update_spec(
    object = object,
    parameters = parameters,
    args_enquo_list = args,
    fresh = fresh,
    cls = "nearest_neighbor_adaptive",
    ...
  )
}

#' @title Check arguments of a model specification.
#' @description Errors when main arguments are set that the selected engine
#' cannot use. Called by parsnip during fit().
#' @param object A model specification.
#' @param call The environment used for error reporting.
#' @return The model specification, invisibly.
#' @method check_args nearest_neighbor_adaptive
#' @importFrom parsnip check_args
#' @keywords internal
#' @export
check_args.nearest_neighbor_adaptive <- function(object,
                                                 call = rlang::caller_env()) {
  if (identical(object$engine, "dann")) {
    # The dann engine has no counterpart for these, so parsnip would
    # otherwise drop them without saying anything.
    unusable <- c("weighted", "sphere", "num_comp")
    is_set <- !vapply(
      object$args[unusable], parsnip::null_value, logical(1)
    )
    unused <- unusable[is_set]
    if (length(unused) > 0) {
      rlang::abort(
        paste0(
          "The following argument(s) are not used by the 'dann' engine: ",
          paste0(unused, collapse = ", "), ". ",
          "They are only available for the 'sub_dann' engine."
        ),
        call = call
      )
    }
  }

  invisible(object)
}
