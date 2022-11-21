test_that("test dual filter", {
  scores <- c(feature1 = 0.25, feature2 = 0.1, feature3 = 0.5, feature4 = 0.9)

  # excludes features 1 and 2
  excluded <- dual_filter(scores, top_p = 2, threshold = NA, cutoff = NA, maximize = TRUE)
  expect_setequal(excluded, c("feature1", "feature2"))

  # excludes feature 2 (score is < cutoff and not in top two features)
  excluded <- dual_filter(scores, top_p = 2, threshold = NA, cutoff = 0.2, maximize = TRUE)
  expect_equal(excluded, "feature2")

  # expect error if both top_p and threshold are used
  expect_error(
    dual_filter(scores, top_p = 2, threshold = 0.5, cutoff = 0.2, maximize = TRUE),
    regexp = "mutually exclusive"
  )

  # excludes features 1-3 because their absolute scores are less than cutoff
  excluded <- dual_filter(scores, top_p = NA, threshold = NA, cutoff = 0.7, maximize = TRUE)
  expect_setequal(excluded, c("feature1", "feature2", "feature3"))

  # excludes features 4 because their absolute scores are greater than cutoff
  excluded <- dual_filter(scores, top_p = NA, threshold = NA, cutoff = 0.7, maximize = FALSE)
  expect_equal(excluded, "feature4")
})
