#' Print dann model.
#'
#' @param x a dann model.
#' @param ... arguments passed to other methods.
#' @examples
#'
#' library(tidymodels)
#' library(tidydann)
#'
#' data("two_class_dat", package = "modeldata")
#' set.seed(1)
#' example_split <- initial_split(two_class_dat, prop = 0.99)
#' example_train <- training(example_split)
#' example_test <- testing(example_split)
#'
#' model <- tidy_dann() |>
#'   set_engine("dann")
#'
#' print(model)
#'
#' @export
print.tidy_dann <- function(x, ...) {
  msg <- paste("tidy dann Model Specification (classification)", "\n")
  cat(msg)

  cat("\n")
  msg <- paste("Main Arguments:", "\n")
  cat(msg)

  params <- x$args
  for (i in seq_len(length(x$args))) {
    paramName <- names(x$args[i])
    paramValue <- rlang::quo_get_expr(rlang::as_quosures(x$args[i][1])[[1]]) # can be tune()
    if (is.null(paramValue)) {
      paramValue <- "engine default"
    }

    msg <- paste("  ", paramName, ": ", paramValue, "\n", collapse = "", sep = "")
    cat(msg)
  }
  cat("\n")

  if (!is.null(x$engine)) {
    msg <- paste("Computational engine:", x$engine, "\n", collapse = " ")
    cat(msg)
    cat("\n")
  }

  invisible(x)
}

#' Print dann model.
#'
#' @param x a dann model.
#' @param ... arguments passed to other methods.
#' @examples
#'
#' library(tidymodels)
#' library(tidydann)
#'
#' data("two_class_dat", package = "modeldata")
#' set.seed(1)
#' example_split <- initial_split(two_class_dat, prop = 0.99)
#' example_train <- training(example_split)
#' example_test <- testing(example_split)
#'
#' model <- tidy_dann() |>
#'   set_engine("dann") |>
#'   fit(formula = Class ~ A + B, data = example_train)
#'
#' print(model)
#'
#' @export
print._dann <- function(x, ...) {
  msg <- paste("parsnip model object", "\n")
  cat(msg)
  cat("\n")

  print(x$spec)
  invisible(x)
}

#' Print sub dann model.
#'
#' @param x a dann model.
#' @param ... arguments passed to other methods.
#' @examples
#'
#' library(tidymodels)
#' library(tidydann)
#'
#' data("two_class_dat", package = "modeldata")
#' set.seed(1)
#' example_split <- initial_split(two_class_dat, prop = 0.99)
#' example_train <- training(example_split)
#' example_test <- testing(example_split)
#'
#' model <- tidy_sub_dann() |>
#'   set_engine("sub_dann")
#'
#' print(model)
#'
#' @export
print.tidy_sub_dann <- function(x, ...) {
  msg <- paste("tidy sub dann Model Specification (classification)", "\n")
  cat(msg)

  cat("\n")
  msg <- paste("Main Arguments:", "\n")
  cat(msg)

  params <- x$args
  for (i in seq_len(length(x$args))) {
    paramName <- names(x$args[i])
    paramValue <- rlang::quo_get_expr(rlang::as_quosures(x$args[i][1])[[1]]) # can be tune()
    if (is.null(paramValue)) {
      paramValue <- "engine default"
    }

    msg <- paste("  ", paramName, ": ", paramValue, "\n", collapse = "", sep = "")
    cat(msg)
  }
  cat("\n")

  if (!is.null(x$engine)) {
    msg <- paste("Computational engine:", x$engine, "\n", collapse = " ")
    cat(msg)
    cat("\n")
  }

  invisible(x)
}

#' Print sub dann model.
#'
#' @param x a dann model.
#' @param ... arguments passed to other methods.
#' @examples
#'
#' library(tidymodels)
#' library(tidydann)
#'
#' data("two_class_dat", package = "modeldata")
#' set.seed(1)
#' example_split <- initial_split(two_class_dat, prop = 0.99)
#' example_train <- training(example_split)
#' example_test <- testing(example_split)
#'
#' model <- tidy_sub_dann() |>
#'   set_engine("sub_dann") |>
#'   fit(formula = Class ~ A + B, data = example_train)
#'
#' print(model)
#'
#' @export
print._sub_dann <- function(x, ...) {
  msg <- paste("parsnip model object", "\n")
  cat(msg)
  cat("\n")

  print(x$spec)
  invisible(x)
}
