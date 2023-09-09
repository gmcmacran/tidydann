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
tunable(dann_spec)

# CV fails
set.seed(452)
cv <- vfold_cv(example_train)
dan_tune_res <- dann_spec |>
  tune_grid(Class ~ A + B, cv, grid = 4)

# Add function to global environment
tunable.tidy_dann <- function(x, ...) {
  tibble::tibble(
    name = c("neighbors", "neighborhood", "epsilon"),
    call_info = list(list(pkg = "dials", fun = "neighbors"), list(pkg = "tidydann", fun = "neighborhood"), list(pkg = "tidydann", fun = "epsilon")),
    source = "model_spec",
    component = "tidy_dann",
    component_id = "main"
  )
}

# Why does it work now?
dan_tune_res <- dann_spec |>
  tune_grid(Class ~ A + B, cv, grid = 4)
