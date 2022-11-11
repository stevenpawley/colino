test_that("discretize_var returns expected values", {
  expect_equal(discretize_var(c(8, 7, 2, 5, NA, 3, 1), cutpoint = 0.5),
               as.factor(c('h', 'h', 'l', 'h', NA, 'l', 'l')))

  expect_equal(discretize_var(c(1, 1, 1, 1, 1, 21), cutpoint = 0.5),
               as.factor(c('l', 'l', 'l', 'l', 'l', 'h')))

  expect_equal(discretize_var(1:50, cutpoint = 0.5),
               as.factor(c(rep('l', 25), rep('h', 25))))

  expect_equal(discretize_var(as.numeric(c(NA, NA, NA, NA, NA)), cutpoint = 0.5),
               as.factor(c(NA, NA, NA, NA, NA)))
})

test_that("discretize_var rejects bad feature input", {
  expect_error(discretize_var(c(NA, NULL), cutpoint = 0.5),
               "Feature must be numeric to discretize")

  expect_error(discretize_var(c('putty', 'grass', 'grass'), cutpoint = 0.5),
               "Feature must be numeric to discretize")

  expect_error(discretize_var(data.frame(x = 1:50), cutpoint = 0.5),
               "Feature must be numeric to discretize")

  expect_error(discretize_var(list(1, 2, 3, 4), cutpoint = 0.5),
               "Feature must be numeric to discretize")
})
