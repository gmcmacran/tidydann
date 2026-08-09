# nocov start

# ------------------------------------------------------------------------------

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

# The registration lives in parsnip's namespace, not ours, so it survives a
# load_all(). Without this guard a second load_all() in the same session errors
# with "Model 'nearest_neighbor_adaptive' already exists", which forces a
# restart between devtools calls.

#' @keywords internal
.onLoad <- function(libname, pkgname) {
  dev_mode <- TRUE

  if (dev_mode) {
    registered <-
      "nearest_neighbor_adaptive" %in% parsnip::get_from_env("models")
    if (!registered) {
      make_nearest_neighbor_adaptive_dann()
      make_nearest_neighbor_adaptive_sub_dann()
    }
  } else {
    make_nearest_neighbor_adaptive_dann()
    make_nearest_neighbor_adaptive_sub_dann()
  }
}

# nocov end
