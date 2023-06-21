#' Feature selection step using a model's feature importance scores or
#' coefficients
#'
#' `step_select_vip` creates a *specification* of a recipe step that selects a
#' subset of predictors based on the ranking of variable importance provided by
#' a `parsnip` model specification and the `model` parameter
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See selections() for more details. For the tidy
#'   method, these are not currently used.
#' @param outcome A character string with the name of the response variable to
#'   use to calculate the feature importance scores.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for preprocessing have
#'   been estimated.
#' @param model A `model_spec` object from `parsnip` that has a feature
#'   importances or coefficients method. The model needs to have an equivalent
#'   `pull_importances` method defined. See `?pull_importances` for how to
#'   define methods for models that are not currently supported.
#' @param top_p An integer with the number of best scoring features to
#'   select.
#' @param threshold A numeric value between 0 and 1 representing the percentile
#'   of best scoring features to select. For example `threshold = 0.9` will
#'   retain only predictors with scores in the top 90th percentile and a smaller
#'   threshold will select more features. Note that `top_p` and `threshold` are
#'   mutually exclusive but either can be used in conjunction with `cutoff` to
#'   select the top-ranked features and those that have filter scores that are
#'   larger than the cutoff value.
#' @param cutoff A numeric value where predictors with _larger_ absolute filter
#'   scores than the cutoff will be retained. A value of `NA` implies that this
#'   criterion will be ignored.
#' @param exclude A character vector of predictor names that will be removed
#'  from the data. This will be set when `prep()` is used on the recipe and
#'  should not be set by the user.
#' @param scores A tibble with 'variable' and 'scores' columns containing the
#'   names of the variables and their feature importance scores. This parameter
#'   is only produced after the recipe has been trained.
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   bake.recipe()? While all operations are baked when prep.recipe() is run,
#'   some operations may not be able to be conducted on new data (e.g.
#'   processing the outcome variable(s)). Care should be taken when using skip =
#'   TRUE as it may affect the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#'
#' @return a `step_select_vip` object.
#' @export
#'
#' @details
#' The recipe will stop if all of `top_p`, `threshold` and `cutoff` are left
#' unspecified.
#'
#' @examples
#' library(recipes)
#' library(parsnip)
#'
#' # load the example cells dataset
#' data(cells, package = "modeldata")
#'
#' # define a base model to use for feature importances
#' base_model <- rand_forest(mode = "classification") %>%
#'     set_engine("ranger", importance = "permutation")
#'
#' # create a preprocessing recipe
#' rec <-
#'  recipe(class ~ ., data = cells[, -1]) %>%
#'  step_select_vip(
#'    all_predictors(),
#'    outcome = "class",
#'    model = base_model,
#'    top_p = 10,
#'    threshold = 0.9
#'  )
#'
#' prepped <- prep(rec)
#'
#' preproc_data <- juice(prepped)
#' prepped
step_select_vip <- function(
  recipe,
  ...,
  outcome = NULL,
  role = "predictor",
  trained = FALSE,
  model = NULL,
  top_p = NA,
  threshold = NA,
  cutoff = NA,
  exclude = NULL,
  scores = NULL,
  skip = FALSE,
  id = recipes::rand_id("select_vip")) {

  if (missing(model))
    rlang::abort("Model argument should be a `parsnip` model specification")

  recipes::add_step(
    recipe,
    step_select_vip_new(
      terms = recipes::ellipse_check(...),
      trained = trained,
      outcome = outcome,
      role = role,
      model = model,
      top_p = top_p,
      threshold = threshold,
      cutoff = cutoff,
      exclude = exclude,
      scores = scores,
      skip = skip,
      id = id
    )
  )
}

# wrapper around 'step' function that sets the class of new step objects
#' @importFrom recipes step
step_select_vip_new <- function(terms, role, trained, outcome, model, top_p,
                                threshold, cutoff, exclude, scores, skip, id) {
  recipes::step(
    subclass = "select_vip",
    terms = terms,
    role = role,
    trained = trained,
    outcome = outcome,
    model = model,
    top_p = top_p,
    threshold = threshold,
    cutoff = cutoff,
    exclude = exclude,
    scores = scores,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_select_vip <- function(x, training, info = NULL, ...) {

  # translate the terms arguments
  x_names <- recipes::recipes_eval_select(x$terms, training, info)
  y_name <- recipes::recipes_eval_select(x$outcome, training, info)
  y_name <- y_name[1]

  # check criteria
  check_criteria(x$top_p, x$threshold, match.call())
  check_zero_one(x$threshold)
  x$top_p <- check_top_p(x$top_p, length(x_names))

  if (length(x_names) > 0) {
    # fit initial model
    X <- training[, x_names]
    y <- training[[y_name]]

    initial_model <- parsnip::fit_xy(x$model, X, y)
    res <- pull_importances(initial_model)
    names(res) <- c("variable", "score")
    res$score <- rlang::set_names(res$score, res$variable)

    exclude <-
      dual_filter(res$score, x$top_p, x$threshold, x$cutoff, maximize = TRUE)

  } else {
    exclude <- character()
  }

  step_select_vip_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    outcome = y_name,
    model = x$model,
    top_p = x$top_p,
    threshold = x$threshold,
    cutoff = x$cutoff,
    exclude = exclude,
    scores = res,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_select_vip <- function(object, new_data, ...) {
  if (length(object$exclude) > 0) {
    new_data <- new_data[, !colnames(new_data) %in% object$exclude]
  }
  as_tibble(new_data)
}

#' @export
print.step_select_vip <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Variable importance feature selection")

    if (recipes::is_trained(x)) {
      n <- length(x$exclude)
      cat(paste0(" (", n, " excluded)"))
    }
    cat("\n")

    invisible(x)
  }

#' @rdname step_select_vip
#' @param x A `step_select_vip` object
#' @param type A character with either 'terms' (the default) to return a
#'   tibble containing the variables that have been removed by the filter step,
#'   or 'scores' to return the scores for each variable.
#' @export
tidy.step_select_vip <- function(x, type = "terms", ...) {
  tidy_filter_step(x, type)
}

#' @export
tunable.step_select_vip <- function(x, ...) {
  tibble(
    name = c("top_p", "threshold", "cutoff"),
    call_info = list(
      list(pkg = "colino", fun = "top_p"),
      list(pkg = "dials", fun = "threshold", range = c(0, 1)),
      list(pkg = "colino", fun = "cutoff")
    ),
    source = "recipe",
    component = "step_select_vip",
    component_id = x$id
  )
}

#' @rdname required_pkgs.colino
#' @export
required_pkgs.step_select_vip <- function(x, ...) {
  c("colino")
}
