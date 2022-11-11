library(recipes)

data(iris)

test_that("basic usage: expected columns retrieved", {
  skip_if_not_installed("FCBF")

  my_iris <- iris
  my_iris[['lglfeat']] <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE)
  my_iris[['partial_NAfeat']] <- c(2, 3, 6, 4, 3, NA)

  rec <-
    recipe(Species ~ ., data = my_iris) %>%
    step_select_fcbf(all_predictors(), min_su = 0.001)

  rec_p <- prep(rec, training = my_iris)

  iris_bake <- bake(rec_p, new_data = my_iris)

  expect_equal(names(iris_bake),
               c("Sepal.Width", "Petal.Width", "Species"))
})

test_that("warns/breaks if not enough predictors are provided", {
  skip_if_not_installed("FCBF")

  # warn if one usable predictor is provided to fcbf
  rec1 <-
    recipe(Species ~ Sepal.Length, iris) %>%
    step_select_fcbf(all_predictors())

  expect_warning(prep(rec1, training = iris), "Only one usable")

  # stop if no usable predictors are provided to fcbf
  rec2 <-
    recipe(Species ~ ., iris[, 'Species', drop = FALSE]) %>%
    step_select_fcbf(all_predictors())

  expect_error(prep(rec2, training = iris), "No usable predictors")
})

test_that("step_select_fcbf rejects bad min_su or cutpoint argument input", {
  skip_if_not_installed("FCBF")

  rec <- recipe(Species ~ ., data = iris)
  error_cut <- "cutpoint must be a number"
  error_su <- "min_su must be a number"

  expect_error(rec %>% step_select_fcbf(min_su = 1.5), error_su)
  expect_error(rec %>% step_select_fcbf(min_su = NA), error_su)
  expect_error(rec %>% step_select_fcbf(min_su = 0), error_su)
  expect_error(rec %>% step_select_fcbf(min_su = "median"), error_su)
  expect_error(rec %>% step_select_fcbf(min_su = TRUE), error_su)
  expect_error(rec %>% step_select_fcbf(min_su = -0.01), error_su)
  expect_error(rec %>% step_select_fcbf(cutpoint = 1.5), error_cut)
  expect_error(rec %>% step_select_fcbf(cutpoint = NA), error_cut)
  expect_error(rec %>% step_select_fcbf(cutpoint = 0), error_cut)
  expect_error(rec %>% step_select_fcbf(cutpoint = "median"), error_cut)
  expect_error(rec %>% step_select_fcbf(cutpoint = TRUE), error_cut)
  expect_error(rec %>% step_select_fcbf(cutpoint = -0.01), error_cut)
})

# return warning if NA columns are provided
test_that("NA columns get removed with warning", {
  skip_if_not_installed("FCBF")

  na_vec <- rep(NA, 10)
  na_dat <- tibble(
    out = rep(c("A", "B"), 5),
    f1 = as.character(na_vec),
    f2 = as.numeric(na_vec),
    f3 = na_vec,
    f4 = c(1, 4, 32, 6, 4, 23, 44, 54, 23, 6),
    f5 = c(1:10)
  )

  inpt_cols <- c('f1', 'f2', 'f3', 'f4', 'f5')

  rec <-
    recipe(out ~ f1 + f2 + f3 + f4 + f5, data = na_dat) %>%
    step_select_fcbf(all_predictors())

  expect_warning(remove_NA_cols(inpt_cols, na_dat), "3 features were full")
  expect_warning(prep(rec, na_dat), "3 features were full")
})


# return error if outcome is not provided, or not in expected format
test_that("bad outcome variables handled correctly", {
  skip_if_not_installed("FCBF")

  # no outcome variable specified in recipe
  rec <-
    recipe(iris) %>%
    update_role(Sepal.Length, Sepal.Width, Petal.Length, new_role =  'predictor') %>%
    step_select_fcbf(all_predictors())

  expect_error(prep(rec, iris), "outcome variable was not found")

  # code works if outcome = argument supplied, despite no outcome in the recipe
  rec2 <-
    recipe(iris) %>%
    update_role(Sepal.Length, Sepal.Width, Petal.Length, new_role =  'predictor') %>%
    step_select_fcbf(all_predictors(), outcome = "Species")

  expect_equal(
    prep(rec2, iris) %>% bake(iris) %>% names,
    c("Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )

  # outcome supplied in unexpected format
  rec3 <-
    recipe(iris) %>%
    update_role(Sepal.Length, Sepal.Width, Petal.Length, new_role =  'predictor') %>%
    step_select_fcbf(all_predictors(), outcome = 5)

  expect_error(prep(rec3, iris), "supplied as a character")

  rec4 <-
    recipe(iris) %>%
    update_role(Sepal.Length, Sepal.Width, Petal.Length, new_role =  'predictor') %>%
    step_select_fcbf(all_predictors(), outcome = c("Species", "Petal.Length"))

  expect_error(prep(rec4, iris), "single outcome variable can be")

  rec5 <-
    recipe(iris) %>%
    update_role(Sepal.Length, Sepal.Width, Petal.Length, new_role =  'predictor') %>%
    step_select_fcbf(all_predictors(), outcome = TRUE)

  expect_error(prep(rec5, iris), "supplied as a character")

  rec6 <-
    recipe(iris) %>%
    update_role(Sepal.Length, Sepal.Width, Petal.Length, new_role =  'predictor') %>%
    step_select_fcbf(all_predictors(), outcome = 'doesnt_exist')

  expect_error(prep(rec6, iris), "not found")

  rec7 <-
    recipe(iris) %>%
    update_role(Sepal.Length, Sepal.Width, Petal.Length, new_role =  'predictor') %>%
    step_select_fcbf(all_predictors(), outcome = NA)

  expect_error(prep(rec7, iris), "outcome variable was not found")
})

# Test if user provides columns by name rather than using tidyselect helpers
test_that("function works if user provides columns by name", {
  skip_if_not_installed("FCBF")

  rec <-
    recipe(Species ~ . , iris) %>%
    step_select_fcbf(c("Petal.Length", "Sepal.Length"))

  expect_equal(
    prep(rec, iris) %>% bake(iris) %>% names,
    c("Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )
})
