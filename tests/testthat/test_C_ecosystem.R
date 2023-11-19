#############################################
# Overview
#
# This script builds both nearest_neighbor_adaptive
# models involving the entire tidymodels ecosystem.
#############################################

suppressPackageStartupMessages(library(recipes))
suppressPackageStartupMessages(library(parsnip))
suppressPackageStartupMessages(library(rsample))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(dials))
suppressPackageStartupMessages(library(tune))
suppressPackageStartupMessages(library(yardstick))
suppressPackageStartupMessages(library(workflows))
suppressPackageStartupMessages(library(tidydann))
suppressPackageStartupMessages(library(dplyr))
library(mlbench)

###############################################
# make data
###############################################
set.seed(1)
circle_data <- mlbench.2dnormals(200, cl = 2, r = sqrt(2), sd = .2) |>
  tibble::as_tibble()
colnames(circle_data) <- c("X1", "X2", "Y")

split <- initial_split(circle_data, prop = .80)
train <- training(split)
test <- testing(split)


###############################################
# helper
###############################################


test_entire_ecosystem <- function(test_case) {
  set.seed(1)

  if (test_case == 1) {
    model <- nearest_neighbor_adaptive(
      neighbors = tune(),
      neighborhood = tune(),
      matrix_diagonal = tune()
    ) |>
      set_engine("dann") |>
      set_mode("classification")

    finalized_neighborhood <- neighborhood() |> get_n_frac(train, frac = .5)
    grid <- grid_random(
      neighbors(),
      finalized_neighborhood,
      matrix_diagonal(),
      size = 5,
      filter = neighbors <= neighborhood
    )
  } else {
    model <-
      nearest_neighbor_adaptive(
        neighbors = tune(),
        neighborhood = tune(),
        matrix_diagonal = tune(),
        weighted = tune(),
        sphere = tune(),
        num_comp = tune()
      ) |>
      set_engine("sub_dann") |>
      set_mode("classification")

    finalized_neighborhood <- neighborhood() |> get_n_frac(train, frac = .5)
    finalized_num_comp <- num_comp() |> get_p(train[-1])
    grid <- grid_random(
      neighbors(),
      finalized_neighborhood,
      matrix_diagonal(),
      weighted(),
      sphere(),
      finalized_num_comp,
      size = 5,
      filter = neighbors <= neighborhood
    )
  }

  rec_obj <- recipe(Y ~ X1 + X2, data = train) |>
    step_center() |>
    step_scale()

  wf <- workflow() |>
    add_model(model) |>
    add_recipe(rec_obj)

  set.seed(1)
  cv <- vfold_cv(data = train, v = 2)
  tune_res <- wf |>
    tune_grid(resamples = cv, grid = grid)
  best_model <- tune_res |>
    select_best(metric = "roc_auc")

  final_model <-
    wf |>
    finalize_workflow(best_model) |>
    last_fit(split)

  final_model |>
    collect_metrics()
}

###############################################
# dann
###############################################
test_that("No errors?", {
  expect_no_error(test_entire_ecosystem(1))
})

###############################################
# sub_dann
###############################################
test_that("No errors?", {
  expect_no_error(test_entire_ecosystem(2))
})
