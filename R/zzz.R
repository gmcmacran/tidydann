#' @keywords internal
.onLoad <- function(libname, pkgname) {
  make_tidy_dann_model()
  make_tidy_sub_dann_model()
}
