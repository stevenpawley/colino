#' Filter Categorical Predictors using the ANOVA F-Test
#'
#' `step_select_aov` creates a *specification* of a recipe step that will filter
#' predictors using their relationship with a numerical outcome as measured
#' using an ANOVA F-test.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which predictors are
#'  affected by the step. See [selections()] for more details. For the `tidy`
#'  method, these are not currently used.
#' @param outcome A single character string that specifies a single numeric
#'  variable.
#' @param role For model terms created by this step, what analysis role should
#'  they be assigned? By default, the function assumes that resulting distances
#'  will be used as predictors in a model.
#' @param cutoff A numeric value, in -log10(p-value) units, where predictors
#'   with _larger_ than the cutoff will be retained. A value of `NA` implies
#'   that this criterion will be ignored.
#' @param top_p An integer that will be used to select the `top_p` predictors
#'   with the smallest p-values. A value of `NA` implies that this criterion
#'   will be ignored.
#' @param threshold A numeric value between 0 and 1 representing the percentile
#'   of best scoring features to select. For example `threshold = 0.9` will
#'   retain only predictors with scores in the top 90th percentile and a smaller
#'   threshold will select more features. Note that `top_p` and `threshold` are
#'   mutually exclusive but either can be used in conjunction with `cutoff` to
#'   select the top-ranked features and those that are smaller than the cutoff
#'   value.
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
#' The recipe will stop if both `top_p`, `threshold` or `cutoff` are left
#' unspecified. If both are used, they are combined via 'or'.
#'
#' @examples
#' data(ames, package = "modeldata")
#'
#' rec <-
#'   recipe(Sale_Price ~ ., data = ames) %>%
#'   step_select_aov(
#'     all_nominal(),
#'     -all_outcomes(),
#'     outcome = "Sale_Price",
#'     top_p = 1,
#'     cutoff = -log10(0.01)
#'   ) %>%
#'   prep()
#'
#' rec %>%
#'   juice(all_nominal()) %>%
#'   names()
#'
#' tidy(rec, number = 1)
step_select_aov <-
  function(recipe,
           ...,
           outcome,
           role = "predictor",
           trained = FALSE,
           top_p = NA,
           threshold = NA,
           cutoff = NA,
           exclude = NULL,
           skip = FALSE,
           id = recipes::rand_id("select_aov")) {
    recipes::add_step(
      recipe,
      step_select_aov_new(
        terms = recipes::ellipse_check(...),
        outcome = outcome,
        role = role,
        trained = trained,
        top_p = top_p,
        threshold = threshold,
        cutoff = cutoff,
        exclude = exclude,
        skip = skip,
        id = id
      )
    )
  }

step_select_aov_new <-
  function(terms, outcome, role, trained, top_p, threshold, cutoff, exclude,
           skip, id) {
    recipes::step(
      subclass = "select_aov",
      terms = terms,
      outcome = outcome,
      role = role,
      trained = trained,
      top_p = top_p,
      threshold = threshold,
      cutoff = cutoff,
      exclude = exclude,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_select_aov <- function(x, training, info = NULL, ...) {
  # get outcome variable
  y_name <- recipes::recipes_eval_select(x$outcome, training, info)
  y_name <- x$outcome[1]

  # check that outcome is numeric
  recipes::check_type(training[, y_name], quant = TRUE)

  # get predictor names
  x_names <- recipes::recipes_eval_select(x$terms, training, info)

  if (length(x_names) > 0) {
    # require only categorical variables
    recipes::check_type(training[, x_names], quant = FALSE)

    # check criteria
    check_criteria(x$top_p, x$threshold, match.call())
    check_zero_one(x$threshold)
    x$top_p <- check_top_p(x$top_p, length(x_names))

    # filter
    f <- paste(y_name, "~", paste(x_names, collapse = " + "))
    f <- as.formula(f)

    scores <- aov(f, training[, c(y_name, x_names)])
    scores <- as_tibble(summary(scores)[[1]], rownames = "term")
    scores <- scores %>%
      dplyr::mutate(term = trimws(term)) %>%
      dplyr::filter(term != "Residuals") %>%
      pull(`Pr(>F)`, name = "term")

    scores <- -log10(scores)

    exclude_chr <- dual_filter(scores, x$top_p, x$threshold, x$cutoff,
                               maximize = TRUE)
  } else {
    exclude_chr <- character()
  }

  step_select_aov_new(
    terms = x$terms,
    outcome = x$outcome,
    role = x$role,
    trained = TRUE,
    top_p = x$top_p,
    threshold = x$threshold,
    cutoff = x$cutoff,
    exclude = exclude_chr,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_select_aov <- function(object, new_data, ...) {
  if (length(object$exclude) > 0) {
    new_data <-
      new_data %>%
      dplyr::select(-dplyr::one_of(object$exclude))
  }
  new_data
}

#' @export
print.step_select_aov <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("ANOVA F-test feature selection")

    if (recipes::is_trained(x)) {
      n <- length(x$exclude)
      cat(paste0(" (", n, " excluded)"))
    }
    cat("\n")

    invisible(x)
  }

#' @rdname step_select_aov
#' @param x A `step_select_aov` object.
#' @export
tidy.step_select_aov <- function(x, ...) {
  tidy_filter_step(x, type = "terms")
}

#' @export
tunable.step_select_aov <- function(x, ...) {
  tibble::tibble(
    name = c("top_p", "threshold", "cutoff"),
    call_info = list(
      list(pkg = "colino", fun = "top_p"),
      list(pkg = "dials", fun = "threshold", range = c(-10, -1)),
      list(pkg = "colino", fun = "cutoff"),
    ),
    source = "recipe",
    component = "step_select_aov",
    component_id = x$id
  )
}

#' @rdname required_pkgs.colino
#' @export
required_pkgs.step_select_aov <- function(x, ...) {
  c("colino")
}
