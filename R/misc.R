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

dual_filter <- function(x, top_p, threshold, maximize) {
  na_x <- x[ is.na(x)]
  x <- x[!is.na(x)]
  x <- sort(x)
  if (maximize) {
    x <- rev(x)
  }
  p <- length(x)

  if (!is.na(top_p)) {
    top_p_lgl <- seq_along(x) <= top_p
  } else {
    top_p_lgl <- rep(FALSE, p)
  }

  if (!is.na(threshold)) {
    if (maximize) {
      threshold_lgl <- x >= threshold
    } else {
      threshold_lgl <- x <= threshold
    }
  } else {
    threshold_lgl <- rep(FALSE, p)
  }
  keep_lgl <- top_p_lgl | threshold_lgl
  c(names(x)[!keep_lgl], names(na_x))
}

select_percentile <- function(x, top_p, threshold, maximize) {
  # filter a named vector by the top_p features or using a percentile
  # threshold

  x <- x[!is.na(x)]

  if (!is.na(threshold)) {
    p_to_exceed <- stats::quantile(x, threshold)

    if (maximize) {
      removals <- x < p_to_exceed
    } else {
      removals <- x >= p_to_exceed
    }

    removals <- names(removals[removals])

  } else {
    if (maximize) {
      x <- sort(x, decreasing = TRUE)
    } else {
      x <- sort(x, decreasing = FALSE)
    }

    removals <- names(x[-seq_len(top_p)])
  }

  removals
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
      filter(role == 'outcome') %>%
      pull(variable)
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
