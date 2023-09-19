suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(parsnip))
suppressPackageStartupMessages(library(recipes))
suppressPackageStartupMessages(library(workflows))
suppressPackageStartupMessages(library(mlbench))
library(dann)

###############################################
# Formula and recipe interface work
###############################################
set.seed(1)
train <- mlbench.2dnormals(1000, cl = 2, r = sqrt(2), sd = .2) |>
  tibble::as_tibble()
colnames(train) <- c("X1", "X2", "Y")


model <- tidy_sub_dann() |>
  set_engine("sub_dann")

rec_obj <- recipe(Y ~ X1 + X2, data = train)
wf <- workflow() |>
  add_model(model) |>
  add_recipe(rec_obj)


test_that("No errors?", {
  expect_no_error(
    tidy_sub_dann() |>
      set_engine("sub_dann") |>
      fit(formula = Y ~ ., data = train)
  )
  expect_no_error(
    wf |>
      fit(data = train)
  )
})

###############################################
# Results match
###############################################

model_01 <- tidy_sub_dann() |>
  set_engine("sub_dann") |>
  fit(formula = Y ~ ., data = train)
pred_01 <- model_01 |>
  predict(new_data = train)

model_02 <- sub_dann(formula = Y ~ ., data = train)
pred_02 <- model_02 |>
  predict(new_data = train)

model_03 <- wf |>
  fit(data = train)
pred_03 <- model_03 |>
  predict(new_data = train)

model_04 <- sub_dann(rec_obj, data = train)
pred_04 <- model_04 |>
  predict(new_data = train)

test_that("Results match?", {
  expect_equal(pred_01, pred_02)
  expect_equal(pred_03, pred_04)
})

rm(train, rec_obj, model, wf)
rm(model_01, model_02, model_03, model_04)
rm(pred_01, pred_02, pred_03, pred_04)
