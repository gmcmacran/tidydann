#' @title Neighborhood size
#'
#' @description The number of nearest neighbors used to estimate the between
#' and within class covariance matrices that shape the neighborhood.
#' @param range A two-element vector holding the defaults for the smallest and
#' largest possible values, respectively. If a transformation is specified,
#' these values should be in the transformed units.
#' @param trans A trans object from the scales package, such as
#' scales::log10_trans() or scales::reciprocal_trans(). If not provided, the
#' default is used, which matches the units used in range. If no
#' transformation, NULL.
#' @return An S3 class of type quant_param from the dials package.
#' @details The upper end of the range depends on the size of the training
#' data, so it is unknown until the data are seen. Use get_n() or finalize()
#' from dials to fill it in.
#'
#' When tuning with cross validation, each model only sees part of the training
#' data. Use get_n_frac() with frac set to 1/V. See the README for a worked
#' example.
#' @examples
#' library(dials)
#' library(tidydann)
#'
#' data("taxi", package = "modeldata")
#' neighborhood() |> finalize(taxi)
#'
#' neighborhood() |> get_n(taxi)
#' @export
neighborhood <- function(range = c(2L, dials::unknown()), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(neighborhood = "# Neighborhood"),
    finalize = dials::get_n
  )
}

#' @title Softening
#'
#' @description Scales the identity matrix added to the between class
#' covariance, which keeps the neighborhood from collapsing onto the class
#' boundary.
#' @inheritParams neighborhood
#' @return An S3 class of type quant_param from the dials package.
#' @details A value of 1 matches the publication. Of the tuning parameters,
#' this one usually has the smallest effect on performance.
#' @examples
#' library(tidydann)
#'
#' matrix_diagonal()
#' @export
matrix_diagonal <- function(range = c(0, 2), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(matrix_diagonal = "# Matrix Diagonal"),
    finalize = NULL
  )
}

#' @title Weighted between class covariance
#'
#' @description Should the between class covariance matrices be weighted?
#' FALSE matches the publication. Only used by the sub_dann engine.
#' @param values A vector of candidate values. Any combination of FALSE and
#' TRUE.
#' @return An S3 class of type qual_param from the dials package.
#' @details Passed to the weighted argument of [fpc::ncoord()].
#' @examples
#' library(tidydann)
#'
#' weighted()
#' @export
weighted <- function(values = c(FALSE, TRUE)) {
  dials::new_qual_param(
    type = "logical",
    values = values,
    label = c(weighted = "Weighted")
  )
}

#' @title Sphering method
#'
#' @description Type of covariance matrix used to sphere the data. Only used
#' by the sub_dann engine.
#' @param values A vector of candidate values. Any combination of "mcd",
#' "mve", "classical", and "none".
#' @return An S3 class of type qual_param from the dials package.
#' @details Passed to the sphere argument of [fpc::ncoord()].
#' @examples
#' library(tidydann)
#'
#' sphere()
#' @export
sphere <- function(values = c("mcd", "mve", "classical", "none")) {
  dials::new_qual_param(
    type = "character",
    values = values,
    label = c(sphere = "Sphere")
  )
}

#' @title Declare tunable parameters
#'
#' @description Returns information on potential hyper-parameters that can be
#' optimized.
#'
#' @param x A model specification of type nearest_neighbor_adaptive.
#' @param ... Other arguments passed to methods.
#' @return A tibble with a column for the parameter name, information on the
#' default method for generating a corresponding parameter object, the source of
#' the parameter (e.g. "recipe", etc.), and the component within the source.
#' @details The result depends on the engine. The dann engine does not use
#' weighted, sphere, or num_comp, so those are omitted for it. When no engine
#' has been set, every parameter is returned.
#' @importFrom generics tunable
#' @export
tunable.nearest_neighbor_adaptive <- function(x, ...) {
  res <- tibble::tibble(
    name = c(
      "neighbors", "neighborhood", "matrix_diagonal",
      "weighted", "sphere", "num_comp"
    ),
    call_info = list(
      list(pkg = "dials", fun = "neighbors"),
      list(pkg = "tidydann", fun = "neighborhood"),
      list(pkg = "tidydann", fun = "matrix_diagonal"),
      list(pkg = "tidydann", fun = "weighted"),
      list(pkg = "tidydann", fun = "sphere"),
      list(pkg = "dials", fun = "num_comp")
    ),
    source = "model_spec",
    component = "nearest_neighbor_adaptive",
    component_id = "main"
  )

  # The dann engine only registers these three main arguments.
  if (identical(x$engine, "dann")) {
    usable <- c("neighbors", "neighborhood", "matrix_diagonal")
    res <- res[res$name %in% usable, ]
  }

  res
}
