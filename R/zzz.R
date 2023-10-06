# nocov start

# ------------------------------------------------------------------------------

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

#' @keywords internal
.onLoad <- function(libname, pkgname) {
  make_nearest_neighbor_adaptive_dann()
  make_nearest_neighbor_adaptive_sub_dann()
}

# nocov end
