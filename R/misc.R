check_zero_one <- function(x) {
  if (is.na(x)) {
    return(x)
  } else {
    if (is.numeric(x)) {
      if (x >= 1 | x <= 0) {
        rlang::abort("`threshold` should be on (0, 1).")
      }
    } else {
      rlang::abort("`threshold` should be numeric.")
    }
  }
  return(x)
}

check_top_p <- function(x, n) {
  # checks on x (top_p) and n (number of features)
  if (is.na(x)) {
    return(x)
  }

  if (!is.numeric(x)) {
    rlang::abort("`top_p` should be numeric.")
  }

  if (!is.integer(x)) {
    x <- as.integer(x)
  }

  msg <- paste0("`top_p` should be on (1, ", n, ") based on the number of features available.")

  # return top_n = all features if top_n > n
  if (x >= n) {
    rlang::warn(msg)
    x <- min(n - 1, x)

  # return a single feature if top_p < 1
  } else if (x < 1) {
    rlang::warn(msg)
    x <- 1
  }

  return(x)
}

check_criteria <- function(top_p, threshold, cl) {
  if (is.na(top_p) & is.na(threshold)) {
    msg <- paste0(
      "For `",
      cl[[1]],
      "`, `top_p` and `threshold` cannot both be missing."
    )
    rlang::abort(msg)
  }
  invisible(NULL)
}

#' Select features using `top_p` or `threshold`.
#'
#' Feature selection using either the `top_p` or `threshold` features OR
#' `cutoff` where cutoff refers to the absolute numeric value of the feature
#' importance scores.
#'
#' @details
#' `dual_filter` selects feature that are selected using either (`top_p`,
#' `threshold`) or `cutoff` or both. If top_p/threshold and cutoff are both used
#' then features are selected using OR. For example, if top_p selects features 1
#' & 2, and threshold selects features 1 & 3, then the selected features =
#' 1,2,3.
#'
#' @param x a named numeric vector of scores per feature
#' @param top_p an integer specifying the number of top-performing features to
#'   retain
#' @param threshold a numeric with percentile of top-performing features to
#'   retain. For example, `threshold = 0.9` will only retain features that are
#'   in the top 90th percentile. A smaller value of threshold will select
#'   more features.
#' @param cutoff a numeric with the value that represents the cutoff in the
#'   scores in `x` by which to retain/discard features.
#' @param maximize logical to indicate whether `top_p`, `threshold` and `cutoff`
#'   are used to keep features where high scores = 'best' (maximize = TRUE) or
#'   where low scores = 'best' (maximize = FALSE).
#'
#' @return character vector of feature names to exclude
#' @keywords internal
dual_filter <- function(x, top_p, threshold, cutoff, maximize) {
  if (!is.na(top_p) & !is.na(threshold)) {
    rlang::abort("`top_p` and `threshold` are mutually exclusive")
  }

  na_x <- x[is.na(x)]
  x <- x[!is.na(x)]
  x <- sort(x, decreasing = maximize)

  p <- length(x)

  # assign logical selection variable using top_p
  if (!is.na(top_p)) {
    top_p_lgl <- seq_along(x) <= top_p
  } else {
    top_p_lgl <- rep(FALSE, p)
  }

  # assign logical selection variable using threshold
  if (!is.na(threshold)) {
    p_to_exceed <- stats::quantile(x, threshold)

    if (maximize) {
      threshold_lgl <- x >= p_to_exceed
    } else {
      threshold_lgl <- x < p_to_exceed
    }

  } else {
    threshold_lgl <- rep(FALSE, p)
  }

  # assign logical selection variable using cutoff
  if (!is.na(cutoff)) {
    if (maximize) {
      cutoff_lgl <- x >= cutoff
    } else {
      cutoff_lgl <- x <= cutoff
    }

  } else {
    cutoff_lgl <- rep(FALSE, p)
  }

  keep_lgl <- top_p_lgl | threshold_lgl | cutoff_lgl
  excluded <- c(names(x)[!keep_lgl], names(na_x))

  return(excluded)
}

check_outcome <- function(y) {
  ifelse(inherits(y, "factor"), "classification", "regression")
}

get_outcome <- function(x, training, info) {
  if (!all(is.na(x$outcome))) {
    if (!all(is.character(x$outcome))) {
      rlang::abort("Outcome variable must be supplied as a character string")
    }

    outcome_col <- x$outcome

  } else {
    outcome_col <- info %>%
      dplyr::filter(.data$role == 'outcome') %>%
      dplyr::pull("variable")
  }

  if (length(outcome_col) > 1) {
    msg <- paste(
      "Multiple outcome variables are present in the recipe.",
      "Only a single outcome variable can be accepted by any `step_select` functions.",
      "Please supply the outcome variable using the `outcome` argument"
    )
    rlang::abort(msg)
  }

  if (length(outcome_col) < 1) {
    msg <- paste(
      "An outcome variable was not found.",
      "Please ensure an outcome variable is specified."
    )
    rlang::abort(msg)
  }

  if (!outcome_col %in% names(training)) {
    rlang::abort(paste0("Outcome variable '", outcome_col, "' not found"))
  }

  return(outcome_col)
}
