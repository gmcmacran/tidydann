library(tidydann)
library(tidymodels)

# Make data to work with
data("two_class_dat", package = "modeldata")
set.seed(4622)
example_split <- initial_split(two_class_dat, prop = 0.99)
example_train <- training(example_split)
example_test <- testing(example_split)

dann_spec <-
  tidy_dann(neighbors = tune(), neighborhood = tune(), epsilon = tune()) |>
  set_engine("dann")

# function works on tidy_dann object
tunable_tidy_dann(dann_spec)

# CV works now
set.seed(452)
cv <- vfold_cv(example_train)
dan_tune_res <- dann_spec |>
  tune_grid(Class ~ A + B, cv, grid = 4)
