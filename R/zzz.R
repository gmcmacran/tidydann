#' @keywords internal
.onLoad <- function(libname, pkgname) {
  make_tidy_dann_model()
  make_tidy_sub_dann_model()

  vctrs::s3_register(
    "generics::tunable", "tidy_dann",
    tunable_tidy_dann
  )
  vctrs::s3_register(
    "generics::tunable", "tidy_sub_dann",
    tunable_tidy_sub_dann
  )
}
