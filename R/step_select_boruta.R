#' Feature selection step using Boruta
#'
#' `step_select_boruta` creates a *specification* of a recipe step that selects
#' a subset of predictors using the Boruta feature selection approach.
#'
#' The Boruta algorithm technically is a wrapper approach that uses random
#' forests to test whether the feature importance scores obtained on the
#' original data are higher than best of the scores obtained when the variables
#' are randomly permuted. These permuted features are termed 'shadow' features.
#' If the scores for any original feature are higher than the best of the scores
#' for the randomly permuted features, then this is marked as a 'hit'. Features
#' are confirmed or rejected based on a confidence threshold (default is p =
#' 0.01) applied to the tails of the binomial distribution with p = 0.5.
#' Features that do not fall within the lower (reject) or upper (accept) tails
#' of the distribution are labelled as 'tentative'. Rejected features are
#' dropped from the feature set and the procedure is repeated until no more
#' 'tentative' features exist, or that a maximum number of runs are reached.
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
#' @param exclude A character vector of predictor names that will be removed
#'   from the data. This will be set when `prep()` is used on the recipe and
#'   should not be set by the user.
#' @param options A list of options to pass to `Boruta::Boruta()`. The defaults
#'   use Boruta's defaults. *Note* that `x` and `y` should not be passed here.
#' @param res The `Boruta::Boruta` object is stored here once this preprocessing
#'   step has been trained by `prep.recipe()`.
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   bake.recipe()? While all operations are baked when prep.recipe() is run,
#'   some operations may not be able to be conducted on new data (e.g.
#'   processing the outcome variable(s)). Care should be taken when using skip =
#'   TRUE as it may affect the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#'
#' @return a `step_select_boruta` object.
#' @export
#' @examples
#' library(recipes)
#' library(parsnip)
#'
#' # load the example iris dataset
#' data(cells, package = "modeldata")
#'
#' # create a preprocessing recipe
#' rec <-
#'  recipe(class ~ ., data = cells[, -1]) %>%
#'  step_select_boruta(all_predictors(), outcome = "class")
#'
#' prepped <- prep(rec)
#'
#' preproc_data <- juice(prepped)
#' prepped
step_select_boruta <- function(
  recipe,
  ...,
  outcome = NULL,
  role = "predictor",
  trained = FALSE,
  exclude = NULL,
  options = list(pValue = 0.01, mcAdj = TRUE, maxRuns = 100),
  res = NULL,
  skip = FALSE,
  id = recipes::rand_id("select_boruta")) {

  recipes::recipes_pkg_check("Boruta")

  recipes::add_step(
    recipe,
    step_select_boruta_new(
      terms = recipes::ellipse_check(...),
      trained = trained,
      outcome = outcome,
      role = role,
      exclude = exclude,
      options = options,
      res = res,
      skip = skip,
      id = id
    )
  )
}

# wrapper around 'step' function that sets the class of new step objects
#' @importFrom recipes step
step_select_boruta_new <- function(terms, role, trained, outcome, exclude,
                                   options, res, skip, id) {
  recipes::step(
    subclass = "select_boruta",
    terms = terms,
    role = role,
    trained = trained,
    outcome = outcome,
    exclude = exclude,
    options = options,
    res = res,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_select_boruta <- function(x, training, info = NULL, ...) {

  # translate the terms arguments
  x_names <- recipes::recipes_eval_select(x$terms, training, info)
  y_name <- recipes::recipes_eval_select(x$outcome, training, info)
  y_name <- y_name[1]

  if (length(x_names) > 0) {

    call <- rlang::call2(
      .fn = "Boruta",
      .ns = "Boruta",
      x = rlang::quo(training[, x_names]),
      y = rlang::quo(training[[y_name]]),
      !!!x$options
    )

    res <- rlang::eval_tidy(call)

    exclude <- names(res$finalDecision[res$finalDecision == "Rejected"])

  } else {
    exclude <- character()
  }

  step_select_boruta_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    outcome = y_name,
    exclude = exclude,
    options = x$options,
    res = res,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_select_boruta <- function(object, new_data, ...) {
  if (length(object$exclude) > 0) {
    new_data <- new_data[, !colnames(new_data) %in% object$exclude]
  }
  as_tibble(new_data)
}

#' @export
print.step_select_boruta <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Boruta feature selection")

    if (recipes::is_trained(x)) {
      n <- length(x$exclude)
      cat(paste0(" (", n, " excluded)"))
    }
    cat("\n")

    invisible(x)
  }

#' @rdname step_select_boruta
#' @param x A `step_select_boruta` object.
#' @param type A character with either 'terms' (the default) to return a
#'   tibble containing the variables that have been removed by the filter step,
#'   or 'scores' to return the scores for each variable.
#' @export
tidy.step_select_boruta <- function(x, type = "terms", ...) {
  tidy_filter_step(x, type)
}

#' @rdname required_pkgs.colino
#' @export
required_pkgs.step_select_boruta <- function(x, ...) {
  c("colino", "Boruta")
}
