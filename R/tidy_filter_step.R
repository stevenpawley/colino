tidy_filter_step <- function(x, type = "terms") {
  if (recipes::is_trained(x)) {
    if (type == "terms") {
      res <- tibble(terms = x$exclude)
    } else if (type == "scores") {
      res <- x$scores
      res <- res[order(res$score, decreasing = TRUE), ]
    }

  } else {
    res <- tibble(terms = rlang::na_chr)
  }
  res$id <- x$id
  res
}
