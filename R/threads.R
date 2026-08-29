#' @title Control the number of threads tidydann uses
#'
#' @description Get and set the number of threads the dann and sub_dann
#' engines use when predicting. These are thin wrappers around
#' [dann::dann_set_threads()], [dann::dann_get_threads()], and
#' [dann::dann_has_openmp()].
#' @param n The number of threads to use. A positive whole number, or NULL to
#' restore the default.
#' @return `tidydann_set_threads` returns the previous setting invisibly: a
#' positive whole number, or NULL if dann was using the default.
#' `tidydann_get_threads` returns the number of threads the next prediction will
#' use. `tidydann_has_openmp` returns TRUE if dann was compiled with OpenMP.
#' @details On Linux and Windows, the prediction loop inside the dann package is
#' parallelized with OpenMP. By default it uses every core the OpenMP runtime
#' makes available. These functions change that count for the dann package
#' alone, so no other package that uses OpenMP is affected. The setting lasts
#' for the R session and is not stored on model objects.
#'
#' This allows the user to move threading from a single model's predict to tidy
#' model's tune or other logic. Thread usage can be changed without refitting.
#'
#' `n` is clamped to the number of threads the OpenMP runtime makes available,
#' with a message. Without OpenMP support, prediction runs on a single thread,
#' `tidydann_get_threads` returns 1 no matter what was set, and
#' `tidydann_has_openmp` returns FALSE.
#'
#' @examples
#' library(tidydann)
#'
#' # Limit dann to two threads.
#' previous <- tidydann_set_threads(2)
#' tidydann_get_threads()
#'
#' # Put it back.
#' tidydann_set_threads(previous)
#'
#' tidydann_has_openmp()
#' @export
tidydann_set_threads <- function(n = NULL) {
  dann::dann_set_threads(n)
}

#' @rdname tidydann_set_threads
#' @export
tidydann_get_threads <- function() {
  dann::dann_get_threads()
}

#' @rdname tidydann_set_threads
#' @export
tidydann_has_openmp <- function() {
  dann::dann_has_openmp()
}
