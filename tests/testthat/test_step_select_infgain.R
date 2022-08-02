library(testthat)
library(recipes)
library(tibble)
library(modeldata)

data("iris")

test_that("step_select_infgain, classification", {
  skip_if_not_installed("FSelectorRcpp")

  irisX <- iris[-5]
  y <- iris$Species

  ig_scores <- as_tibble(FSelectorRcpp::information_gain(x = irisX, y = y))
  ig_scores <- ig_scores[order(ig_scores$importance), ]
  ig_scores$importance <- rlang::set_names(ig_scores$importance, ig_scores$attributes)
  ig_scores <- ig_scores[order(ig_scores$importance, decreasing = TRUE), ]

  rec <- recipe(Species ~ ., data = iris)

  ig_rec <- rec %>%
    step_select_infgain(
      all_predictors(), outcome = "Species", type = "infogain", top_p = 2) %>%
    prep()

  ig_pred <- juice(ig_rec)
  expect_true(all(names(ig_pred)[1:2] %in% ig_scores$attributes[1:2]))
})


test_that("step_select_infgain, regression", {
  skip_if_not_installed("FSelectorRcpp")
  data("biomass", package = "modeldata")

  X <- as.data.frame(biomass[, -c(1:2, 8)])
  y <- biomass$HHV

  ig_scores <-
    as_tibble(FSelectorRcpp::information_gain(x = X, y = y, equal = TRUE))
  ig_scores <- ig_scores[order(ig_scores$importance), ]
  ig_scores$importance <- rlang::set_names(ig_scores$importance, ig_scores$attributes)
  ig_scores <- ig_scores[order(ig_scores$importance, decreasing = TRUE), ]

  ig_rec <-
    recipe(HHV ~ ., data = biomass[, -(1:2)]) %>%
    step_select_infgain(
      all_predictors(),
      outcome = "HHV",
      type = "infogain",
      top_p = 2) %>%
    prep()

  ig_pred <- bake(ig_rec, new_data = NULL)
  expect_equal(names(ig_pred)[1:2], ig_scores$attributes[1:2])

  tidyed_scores <- tidy(ig_rec, number = 1, type = "scores")
  tidyed_scores <- tidyed_scores[, -3]
  expect_equal(tidyed_scores$variable, ig_scores$attributes)
  expect_equal(tidyed_scores$score, ig_scores$importance)
})
