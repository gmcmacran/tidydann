suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(parsnip))
suppressPackageStartupMessages(library(rsample))
library(tidydann)

###############################################
# Make data to work with
###############################################
data("two_class_dat", package = "modeldata")
set.seed(1)
example_split <- initial_split(two_class_dat, prop = 0.99)
example_train <- training(example_split)
example_test <- testing(example_split)

###############################################
# Test dann's print
###############################################
m1 <- tidy_dann()

m2 <- tidy_dann(neighbors = 2, neighborhood = 3, epsilon = 1.5)

m3 <- tidy_dann(neighbors = tune(), neighborhood = tune(), epsilon = tune())

m4 <- tidy_dann() |>
  set_engine("dann")

m5 <- tidy_dann(neighbors = 2, neighborhood = 3, epsilon = 1.5) |>
  set_engine("dann")

m6 <- tidy_dann(neighbors = tune(), neighborhood = tune(), epsilon = tune()) |>
  set_engine("dann")

m7 <- tidy_dann(neighbors = 2, neighborhood = 3, epsilon = 1.5) |>
  set_engine("dann") |>
  fit(Class ~ A + B, data = example_train)

test_that("", {
  expect_output(print(m1))
  expect_output(print(m2))
  expect_output(print(m3))
  expect_output(print(m4))
  expect_output(print(m5))
  expect_output(print(m6))
  expect_output(print(m7))
})

###############################################
# Test sub dann's print
###############################################
m1 <- tidy_sub_dann()

m2 <- tidy_sub_dann(
  neighbors = 2, neighborhood = 3, epsilon = 1.5,
  weighted = TRUE, sphere = "mcd", num_comp = 2
)

m3 <- tidy_sub_dann(
  neighbors = tune(), neighborhood = tune(), epsilon = tune(),
  weighted = tune(), sphere = tune(), num_comp = tune()
)

m4 <- tidy_sub_dann() |>
  set_engine("sub_dann")

m5 <- tidy_sub_dann(
  neighbors = 2, neighborhood = 3, epsilon = 1.5,
  weighted = TRUE, sphere = "mcd", num_comp = 2
) |>
  set_engine("sub_dann")

m6 <- tidy_sub_dann(
  neighbors = tune(), neighborhood = tune(), epsilon = tune(),
  weighted = tune(), sphere = tune(), num_comp = tune()
) |>
  set_engine("sub_dann")

m7 <- tidy_sub_dann(neighbors = 2, neighborhood = 3, epsilon = 1.5) |>
  set_engine("sub_dann") |>
  fit(Class ~ A + B, data = example_train)

test_that("", {
  expect_output(print(m1))
  expect_output(print(m2))
  expect_output(print(m3))
  expect_output(print(m4))
  expect_output(print(m5))
  expect_output(print(m6))
  expect_output(print(m7))
})
