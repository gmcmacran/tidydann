#############################################
# Overview
#
# This script tests the thread control wrappers
# around the dann package.
#############################################

suppressPackageStartupMessages(library(tidydann))

############
# get and set round trip
############
test_that("setting changes the count", {
  previous <- tidydann_set_threads(1)
  expect_equal(tidydann_get_threads(), 1)

  tidydann_set_threads(previous)
})

test_that("set returns the previous setting invisibly", {
  start <- tidydann_set_threads(1)

  expect_invisible(tidydann_set_threads(2))
  expect_equal(tidydann_set_threads(1), 2)

  tidydann_set_threads(start)
})

test_that("NULL restores the default", {
  start <- tidydann_set_threads(1)

  expect_equal(tidydann_set_threads(NULL), 1)
  expect_equal(tidydann_get_threads(), dann::dann_get_threads())

  tidydann_set_threads(start)
})

############
# matches dann
############
test_that("wrappers match dann", {
  start <- tidydann_set_threads(1)

  expect_equal(tidydann_get_threads(), dann::dann_get_threads())
  expect_equal(tidydann_has_openmp(), dann::dann_has_openmp())

  tidydann_set_threads(start)
})

test_that("has_openmp returns a flag", {
  expect_true(is.logical(tidydann_has_openmp()))
  expect_length(tidydann_has_openmp(), 1)
  expect_false(is.na(tidydann_has_openmp()))
})

############
# bad inputs are caught by dann
############
test_that("bad inputs error", {
  expect_error(tidydann_set_threads(0))
  expect_error(tidydann_set_threads(-1))
  expect_error(tidydann_set_threads(2.5))
  expect_error(tidydann_set_threads("2"))
  expect_error(tidydann_set_threads(c(1, 2)))
})
