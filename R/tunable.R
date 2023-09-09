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
#' @param x A model specification of type tidy_dann
#' specification.
#' @param ... Other arguments passed to methods.
#' @return A tibble with a column for the parameter name, information on the default method for
#' generating a corresponding parameter object, the source of the parameter (e.g. "recipe", etc.),
#'  and the component within the source.
#' @export
tunable_tidy_dann <- function(x, ...) {
  tibble::tibble(
    name = c("neighbors", "neighborhood", "epsilon"),
    call_info = list(list(pkg = "dials", fun = "neighbors"), list(pkg = "tidydann", fun = "neighborhood"), list(pkg = "tidydann", fun = "epsilon")),
    source = "model_spec",
    component = "tidy_dann",
    component_id = "main"
  )
}
