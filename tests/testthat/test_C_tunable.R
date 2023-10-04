suppressPackageStartupMessages(library(parsnip))
suppressPackageStartupMessages(library(dials))

##############################
# Test data
##############################
data("two_class_dat", package = "modeldata")


##############################
# Test neighborhood
##############################
finalized_neighborhood <- neighborhood() |> get_n(two_class_dat)

test_that("", {
  expect_true(all(class(finalized_neighborhood) == c("quant_param", "param")))
  expect_true(range_get(finalized_neighborhood)$lower == 2)
  expect_true(range_get(finalized_neighborhood)$upper == nrow(two_class_dat))
})

finalized_neighborhood <- neighborhood() |> finalize(two_class_dat)

test_that("", {
  expect_true(all(class(finalized_neighborhood) == c("quant_param", "param")))
  expect_true(range_get(finalized_neighborhood)$lower == 2)
  expect_true(range_get(finalized_neighborhood)$upper == nrow(two_class_dat))
})

finalized_neighborhood <- neighborhood() |> get_n_frac(two_class_dat,
  frac = .50
)

test_that("", {
  expect_true(all(class(finalized_neighborhood) == c("quant_param", "param")))
  expect_true(range_get(finalized_neighborhood)$lower == 2)
  expect_true(
    range_get(finalized_neighborhood)$upper == floor(nrow(two_class_dat) / 2)
  )
})


##############################
# Test matrix_diagonal
##############################
test_that("?", {
  expect_true(all(class(matrix_diagonal()) == c("quant_param", "param")))
  expect_true(range_get(matrix_diagonal())$lower == 0)
  expect_true(range_get(matrix_diagonal())$upper == 2)
})

##############################
# Test weighted
##############################
test_that("", {
  expect_true(all(class(weighted()) == c("qual_param", "param")))
  expect_true(all(weighted()$value == c("FALSE", "TRUE")))
})

##############################
# Test sphere
##############################
test_that("", {
  expect_true(all(class(sphere()) == c("qual_param", "param")))
  expect_true(all(sphere()$value == c("mcd", "mve", "classical", "none")))
})

##############################
# Test tunable.tidy_dann
##############################
dann_spec <-
  tidy_dann() |>
  set_engine("dann")
output <- tunable.tidy_dann(dann_spec)

test_that("", {
  expect_true(nrow(output) == 3)
  expect_true(ncol(output) == 5)
  expect_true(all(colnames(output) == c(
    "name", "call_info", "source",
    "component", "component_id"
  )))
})
rm(dann_spec, output)

##############################
# Test tunable.sub_dann
##############################
sub_dann_spec <-
  tidy_sub_dann() |>
  set_engine("sub_dann")
output <- tunable(sub_dann_spec)

test_that("", {
  expect_true(nrow(output) == 6)
  expect_true(ncol(output) == 5)
  expect_true(all(colnames(output) == c(
    "name", "call_info", "source",
    "component", "component_id"
  )))
})
rm(sub_dann_spec, output)
