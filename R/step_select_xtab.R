#' Filter Categorical Predictors using Contingency Tables
#'
#' `step_select_xtab` creates a *specification* of a recipe step that will
#'  filter predictors using their relationship with the outcome as measured
#'  using statistical tests for association.
#'
#' @param recipe 	A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which predictors are
#'  affected by the step. See [selections()] for more details. For the `tidy`
#'  method, these are not currently used.
#' @param outcome A single character string that specifies a single categorical
#'  variable to be used as the class.
#' @param role For model terms created by this step, what analysis role should
#'  they be assigned?. By default, the function assumes that resulting distances
#'  will be used as predictors in a model.
#' @param top_p An integer that will be used to select the predictors with the
#'  smallest p/FDR values. A value of `NA` implies that this criterion will be
#'  ignored.
#' @param threshold A numeric value between 0 and 1 representing the percentile
#'   of best scoring features to select. For example `threshold = 0.9` will
#'   retain only predictors with scores in the top 90th percentile and a smaller
#'   threshold will select more features. Note that `top_p` and `threshold` are
#'   mutually exclusive but either can be used in conjunction with `cutoff` to
#'   select the top-ranked features and those that have filter scores that are
#'   larger than the cutoff value.
#' @param cutoff A numeric value, in p-value/FDR units, where predictors with
#'  _smaller_ than the threshold will be retained. A value of `NA`
#'  implies that this criterion will be ignored.
#' @param exact Should an exact test be used?
#' @param fdr Should false discovery rates (FDR) be used instead of p-values?
#' @param exclude A character vector of predictor names that will be removed
#'  from the data. This will be set when `prep()` is used on the recipe and
#'  should not be set by the user.
#' @param trained A logical to indicate if the quantities for preprocessing have
#'   been estimated.
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   bake.recipe()? While all operations are baked when prep.recipe() is run,
#'   some operations may not be able to be conducted on new data (e.g.
#'   processing the outcome variable(s)). Care should be taken when using skip =
#'   TRUE as it may affect the computations for subsequent operations.
#' @param id 	A character string that is unique to this step to identify it.
#' @return An updated version of `recipe` with the new step added to the
#'  sequence of existing steps (if any). For the `tidy` method, a tibble with a
#'  `terms` column for which predictors were removed.
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
#'                    top_p = 1, threshold = 0.001, exact = TRUE) %>%
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
