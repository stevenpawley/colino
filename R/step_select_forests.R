#' Feature selection step using a random forest feature importance scores
#'
#' `step_select_forests` creates a *specification* of a recipe step that selects
#' a subset of predictors based on the ranking of variable importance using a
#' `parsnip::rand_forest` supported model.
#'
#' @inheritParams step_select_aov
#' @inherit step_select_aov return
#' @param outcome A character string with the name of the response variable to
#'   use to calculate the feature importance scores.
#' @param role Not used by this step since no new variables are created.
#' @param engine A supported rand_forest engine that is supported by parsnip.
#'   The default is "ranger".
#' @param options A named list of options to pass to the rand_forest engine. For
#'   example, if `engine = 'ranger'` (the default) then options could be
#'   `list(permutation = 'importance`) because a feature importance method needs
#'   to be specified for this engine. This is the default.
#' @param mtry An integer for the number of predictors that will be randomly
#'   sampled at each split when creating the tree models.
#' @param trees An integer for the number of trees contained in the ensemble.
#' @param min_n An integer for the minimum number of data points in a node that
#'   are required for the node to be split further.
#' @param scores A tibble with 'variable' and 'scores' columns containing the
#'   names of the variables and their feature importance scores. This parameter
#'   is only produced after the recipe has been trained.
#'
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
#'  step_select_forests(all_predictors(), outcome = "class", top_p = 10,
#'                      cutoff = 0.9)
#'
#' prepped <- prep(rec)
#'
#' preproc_data <- juice(prepped)
#' prepped
step_select_forests <- function(
    recipe,
    ...,
    outcome = NULL,
    role = "predictor",
    trained = FALSE,
    engine = "ranger",
    options = list(importance = "permutation"),
    mtry = NULL,
    trees = NULL,
    min_n = NULL,
    top_p = NA,
    threshold = NA,
    cutoff = NA,
    exclude = NULL,
    scores = NULL,
    skip = FALSE,
    id = recipes::rand_id("select_forests")) {

  engines <- parsnip::show_engines("rand_forest")$engine

  if (!engine %in% parsnip::show_engines("rand_forest")$engine){

    # make message more clear if someone is trying to use aorsf
    if(engine == 'aorsf'){
      rlang::abort("the bonsai package must be loaded to use aorsf engine")
    }

    rlang::abort(
      paste("Engine argument should be one of", paste(engines, collapse = ", "))
    )

  }


  recipes::add_step(
    recipe,
    step_select_forests_new(
      terms = recipes::ellipse_check(...),
      trained = trained,
      outcome = outcome,
      role = role,
      engine = engine,
      options = options,
      mtry = mtry,
      trees = trees,
      min_n = min_n,
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
step_select_forests_new <- function(terms, role, trained, outcome, engine,
                                    options, top_p, threshold, cutoff, mtry,
                                    trees, min_n, exclude, scores, skip, id) {
  recipes::step(
    subclass = "select_forests",
    terms = terms,
    role = role,
    trained = trained,
    outcome = outcome,
    engine = engine,
    options = options,
    mtry = mtry,
    trees = trees,
    min_n = min_n,
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
prep.step_select_forests <- function(x, training, info = NULL, ...) {
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

    model_mode <- check_outcome(y)

    model_args <- list(
      trees = x$trees,
      mtry = x$mtry,
      min_n = x$min_n
    )

    # aorsf package uses 'permute' instead of 'permutation'
    if(x$engine == 'aorsf' & x$options$importance == 'permutation'){
      x$options$importance <- 'permute'
    }

    model_spec <-
      parsnip::make_call("rand_forest", args = model_args, ns = "parsnip")

    model_spec <-
      rlang::eval_tidy(model_spec) %>%
      parsnip::set_mode(model_mode) %>%
      parsnip::set_engine(x$engine, !!!x$options)

    initial_model <- parsnip::fit_xy(model_spec, X, y)
    res <- pull_importances(initial_model)
    names(res) <- c("variable", "score")
    res$score <- rlang::set_names(res$score, res$variable)

    exclude <-
      dual_filter(res$score, x$top_p, x$threshold, x$cutoff, maximize = TRUE)

  } else {
    exclude <- character()
  }

  step_select_forests_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    outcome = y_name,
    engine = x$engine,
    options = x$options,
    mtry = x$mtry,
    trees = x$trees,
    min_n = x$min_n,
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
bake.step_select_forests <- function(object, new_data, ...) {
  if (length(object$exclude) > 0) {
    new_data <- new_data[, !colnames(new_data) %in% object$exclude]
  }

  as_tibble(new_data)
}

#' @export
print.step_select_forests <-
  function(x, width = max(20, options()$width - 30),
           ...) {
    cat("Variable importance feature selection")

    if (recipes::is_trained(x)) {
      n <- length(x$exclude)
      cat(paste0(" (", n, " excluded)"))
    }
    cat("\n")

    invisible(x)
  }

#' @rdname step_select_forests
#' @param x A `step_select_forests` object.
#' @param type A character with either 'terms' (the default) to return a
#'   tibble containing the variables that have been removed by the filter step,
#'   or 'scores' to return the scores for each variable.
#' @export
tidy.step_select_forests <- function(x, type = "terms", ...) {
  tidy_filter_step(x, type)
}

#' @export
tunable.step_select_forests <- function(x, ...) {
  tibble(
    name = c("top_p", "threshold", "cutoff", "mtry", "trees", "min_n"),
    call_info = list(
      list(pkg = "colino", fun = "top_p"),
      list(pkg = "dials", fun = "threshold", range = c(0, 1)),
      list(pkg = "colino", fun = "cutoff"),
      list(pkg = "dials", fun = "mtry", range = c(1L, dials::unknown())),
      list(pkg = "dials", fun = "trees", range = c(1L, 2000L)),
      list(pkg = "dials", fun = "min_n", range = c(2L, 40L))
    ),
    source = "recipe",
    component = "step_select_forests",
    component_id = x$id
  )
}

#' @rdname required_pkgs.colino
#' @export
required_pkgs.step_select_forests <- function(x, ...) {
  c("colino")
}
