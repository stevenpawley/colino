#' Filter Categorical Predictors using Contingency Tables
#'
#' `step_select_xtab` creates a *specification* of a recipe step that will
#'  filter predictors using their relationship with the outcome as measured
#'  using statistical tests for association.
#'
#' @inheritParams step_select_aov
#' @inherit step_select_aov return
#' @param outcome A single character string that specifies a single categorical
#'  variable to be used as the class.
#' @param role For model terms created by this step, what analysis role should
#'  they be assigned?. By default, the function assumes that resulting distances
#'  will be used as predictors in a model.
#' @param exact Should an exact test be used?
#' @param fdr Should false discovery rates (FDR) be used instead of p-values?
#'
#' @keywords datagen
#' @concept preprocessing
#' @concept supervised_filter
#' @export
#' @details
#'
#' The recipe will stop if all of `top_p`, `threshold` and `cutoff` are left
#' unspecified. If both are used, they are combined via 'or'.
#'
#' The Benjamini-Hochberg FDR correction is used (see [stats::p.adjust()]).
#'
#' Warnings from [stats::chisq.test()] and [stats::fisher.test()] are suppressed.
#' @examples
#' data(attrition, package = "modeldata")
#'
#' rec <-
#'   recipe(Attrition ~ ., data = attrition) %>%
#'   step_select_xtab(all_nominal(), -all_outcomes(), outcome = "Attrition",
#'                    top_p = 1, cutoff = 0.001, exact = TRUE) %>%
#'   prep()
#'
#' rec %>% juice(all_nominal(), -all_outcomes()) %>% names()
#'
#' tidy(rec, number = 1)
step_select_xtab <- function(recipe,
                             ...,
                             outcome,
                             role = "predictor",
                             trained = FALSE,
                             top_p = NA,
                             threshold = NA,
                             cutoff = NA,
                             exact = FALSE,
                             fdr = TRUE,
                             exclude = NULL,
                             skip = FALSE,
                             id = recipes::rand_id("select_xtab")) {
  recipes::add_step(
    recipe,
    step_select_xtab_new(
      terms = recipes::ellipse_check(...),
      outcome = outcome,
      role = role,
      trained = trained,
      top_p = top_p,
      threshold = threshold,
      cutoff = cutoff,
      exact = exact,
      fdr = fdr,
      exclude = exclude,
      skip = skip,
      id = id
    )
  )
}

step_select_xtab_new <-
  function(terms, outcome, role, trained, top_p, threshold, cutoff, exact, fdr,
           exclude, skip, id) {
    recipes::step(
      subclass = "select_xtab",
      terms = terms,
      outcome = outcome,
      role = role,
      trained = trained,
      top_p = top_p,
      threshold = threshold,
      cutoff = cutoff,
      exact = exact,
      fdr = fdr,
      exclude = exclude,
      skip = skip,
      id = id
    )
  }

tbl_calc <- function(x, y, exact) {
  xtab <- table(x, y)
  if (exact) {
    res <- suppressWarnings(try(stats::fisher.test(xtab)$p.value, silent = TRUE))
  } else {
    res <- suppressWarnings(try(stats::chisq.test(xtab)$p.value, silent = TRUE))
  }
  if (inherits(res, "try-error")) {
    res <- NA_real_
  }
  res
}

#' @export
prep.step_select_xtab <- function(x, training, info = NULL, ...) {
  y_name <- recipes::recipes_eval_select(x$outcome, training, info)
  y_name <- x$outcome[1]
  recipes::check_type(training[, y_name], quant = FALSE)
  x_names <- recipes::recipes_eval_select(x$terms, training, info)

  if (length(x_names) > 0) {

    recipes::check_type(training[, x_names], quant = FALSE)

    # check criteria
    check_criteria(x$top_p, x$threshold, match.call())
    check_zero_one(x$threshold)
    x$top_p <- check_top_p(x$top_p, length(x_names))

    # filter
    scores <- purrr::map_dbl(training[, x_names],
                             ~ tbl_calc(.x, training[[y_name]], exact = x$exact))
    scores <- sort(scores, na.last = TRUE)
    if (x$fdr) {
      scores <- stats::p.adjust(scores, method = "BH")
    }

    exclude_chr <- dual_filter(scores, x$top_p, x$threshold, x$cutoff, maximize = FALSE)
  } else {
    exclude_chr <- character()
  }

  step_select_xtab_new(
    terms = x$terms,
    outcome = x$outcome,
    role = x$role,
    trained = TRUE,
    top_p = x$top_p,
    threshold = x$threshold,
    cutoff = x$cutoff,
    exact = x$exact,
    fdr = x$fdr,
    exclude = exclude_chr,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_select_xtab <- function(object, new_data, ...) {
  if (length(object$exclude) > 0) {
    new_data <- new_data %>% dplyr::select(-dplyr::one_of(object$exclude))
  }
  new_data
}

#' @export
print.step_select_xtab <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Association test feature selection")

    if (recipes::is_trained(x)) {
      n <- length(x$exclude)
      cat(paste0(" (", n, " excluded)"))
    }
    cat("\n")

    invisible(x)
  }

#' @rdname step_select_xtab
#' @param x A `step_select_xtab` object.
#' @export
tidy.step_select_xtab <- function(x, ...) {
  tidy_filter_step(x, type = "terms")
}

#' @export
tunable.step_select_xtab <- function(x, ...) {
  tibble::tibble(
    name = c("top_p", "threshold", "cutoff"),
    call_info = list(
      list(pkg = "colino", fun = "top_p"),
      list(pkg = "dials", fun = "threshold", range = c(-10, -1)),
      list(pkg = "colino", fun = "cutoff")
    ),
    source = "recipe",
    component = "step_select_xtab",
    component_id = x$id
  )
}

#' @rdname required_pkgs.colino
#' @export
required_pkgs.step_select_xtab <- function(x, ...) {
  c("colino")
}
