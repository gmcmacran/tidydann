#' @title Neighborhood size
#'
#' @description Number of data points used to calculate shape of the neighborhood.
#' @param range	A two-element vector holding the defaults for the smallest and largest possible values, respectively.
#' If a transformation is specified, these values should be in the transformed units.
#' @param trans	A trans object from the scales package, such as scales::log10_trans() or scales::reciprocal_trans().
#' If not provided, the default is used which matches the units used in range. If no transformation, NULL.
#' @details Use get_p from dials to finalize.
#' @examples
#' library(tidymodels)
#' library(tidydann)
#'
#' data("two_class_dat", package = "modeldata")
#' neighborhood() |> get_p(two_class_dat[-1])
#' @export
neighborhood <- function(range = c(2L, dials::unknown()), trans = NULL) {
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
#' library(tidydann)
#'
#' epsilon()
#' @export
epsilon <- function(range = c(0, 2), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
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
    call_info = list(
      list(pkg = "dials", fun = "neighbors"),
      list(pkg = "tidydann", fun = "neighborhood"),
      list(pkg = "tidydann", fun = "epsilon")
    ),
    source = "model_spec",
    component = "tidy_dann",
    component_id = "main"
  )
}

#' @title Weighted argument to ncoord
#'
#' @param values A two-element vector containing FALSE and TRUE.
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

#' @title Sphere argument to ncoord
#'
#' @param values A four-element vector containing "mcd", "mve", "classical", and "none".
#' @examples
#' library(tidydann)
#'
#' sphere()
#' @export
sphere <- function(values = c("mcd", "mve", "classical", "none")) {
  dials::new_qual_param(
    type = "character",
    values = values,
    label = c(sphere = "Sphere ")
  )
}

#' @title Declare tunable parameters
#'
#' @description Returns information on potential hyper-parameters that can be optimized.
#'
#' @param x A model specification of type tidy_sub_dann
#' specification.
#' @param ... Other arguments passed to methods.
#' @return A tibble with a column for the parameter name, information on the default method for
#' generating a corresponding parameter object, the source of the parameter (e.g. "recipe", etc.),
#'  and the component within the source.
#' @export
tunable_tidy_sub_dann <- function(x, ...) {
  tibble::tibble(
    name = c("neighbors", "neighborhood", "epsilon", "weighted", "sphere", "num_comp"),
    call_info = list(
      list(pkg = "dials", fun = "neighbors"),
      list(pkg = "tidydann", fun = "neighborhood"),
      list(pkg = "tidydann", fun = "epsilon"),
      list(pkg = "tidydann", fun = "weighted"),
      list(pkg = "tidydann", fun = "sphere"),
      list(pkg = "dials", fun = "num_comp")
    ),
    source = "model_spec",
    component = "tidy_sub_dann",
    component_id = "main"
  )
}
