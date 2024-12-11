#' Feature selection step using a decision tree importance scores
#'
#' `step_select_tree` creates a *specification* of a recipe step that selects a
#' subset of predictors based on the ranking of variable importance provided by
#' a `parsnip::decision_tree` supported model.
#'
#' @inheritParams step_select_aov
#' @inherit step_select_aov return
#' @param outcome A character string with the name of the response variable to
#'   use to calculate the feature importance scores.
#' @param role Not used by this step since no new variables are created.
#' @param engine A supported rand_forest engine that is supported by parsnip.
#'   The default is "rpart".
#' @param cost_complexity A positive number for the the cost/complexity
#'   parameter (a.k.a. Cp) used by CART models (specific engines only).
#' @param tree_depth An integer for maximum depth of the tree.
#' @param min_n An integer for the minimum number of data points in a node that
#'   are required for the node to be split further.
#' @param scores A tibble with 'variable' and 'scores' columns containing the
#'   names of the variables and their feature importance scores. This parameter
#'   is only produced after the recipe has been trained.
#'
#' @export
#'
#' @details
#'
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
#' # create a preprocessing recipe
#' rec <-
#'  recipe(class ~ ., data = cells[, -1]) %>%
#'  step_select_tree(all_predictors(), outcome = "class", top_p = 10)
#'
#' prepped <- prep(rec)
#'
#' preproc_data <- bake(prepped, new_data = NULL)
#' prepped
step_select_tree <- function(
    recipe,
    ...,
    outcome = NULL,
    role = "predictor",
    trained = FALSE,
    engine = "rpart",
    cost_complexity = NULL,
    tree_depth = NULL,
    min_n = NULL,
    top_p = NA,
    threshold = NA,
    cutoff = NA,
    exclude = NULL,
    scores = NULL,
    skip = FALSE,
    id = recipes::rand_id("select_tree")) {

  engines <- parsnip::show_engines("decision_tree")$engine

  if (!engine %in% engines) {
    rlang::abort(
      paste("Engine argument should be one of", paste(engines, collapse = ", "))
    )
  }

  recipes::add_step(
    recipe,
    step_select_tree_new(
      terms = recipes::ellipse_check(...),
      trained = trained,
      outcome = outcome,
      role = role,
      engine = engine,
      cost_complexity = cost_complexity,
      tree_depth = tree_depth,
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
step_select_tree_new <- function(terms, role, trained, outcome, engine,
                                 top_p, threshold, cutoff, cost_complexity,
                                 tree_depth, min_n, exclude, scores, skip, id) {
  recipes::step(
    subclass = "select_tree",
    terms = terms,
    role = role,
    trained = trained,
    outcome = outcome,
    engine = engine,
    cost_complexity = cost_complexity,
    tree_depth = tree_depth,
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
prep.step_select_tree <- function(x, training, info = NULL, ...) {

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
      cost_complexity = x$cost_complexity,
      tree_depth = x$tree_depth,
      min_n = x$min_n
    )

    model_spec <-
      parsnip::make_call("decision_tree", args = model_args, ns = "parsnip")

    model_spec <-
      rlang::eval_tidy(model_spec) %>%
      parsnip::set_mode(model_mode) %>%
      parsnip::set_engine(x$engine)

    initial_model <- parsnip::fit_xy(model_spec, X, y)
    res <- pull_importances(initial_model)
    names(res) <- c("variable", "score")
    res$score <- rlang::set_names(res$score, res$variable)

    exclude <-
      dual_filter(res$score, x$top_p, x$threshold, x$cutoff, maximize = TRUE)

  } else {
    exclude <- character()
  }

  step_select_tree_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    outcome = y_name,
    engine = x$engine,
    cost_complexity = x$cost_complexity,
    tree_depth = x$tree_depth,
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
bake.step_select_tree <- function(object, new_data, ...) {
  if (length(object$exclude) > 0) {
    new_data <- new_data[, !colnames(new_data) %in% object$exclude]
  }

  as_tibble(new_data)
}

#' @export
print.step_select_tree <-
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

#' @rdname step_select_tree
#' @param x A `step_select_tree` object.
#' @param type A character with either 'terms' (the default) to return a
#'   tibble containing the variables that have been removed by the filter step,
#'   or 'scores' to return the scores for each variable.
#' @export
tidy.step_select_tree <- function(x, type = "terms", ...) {
  tidy_filter_step(x, type)
}

#' @export
tunable.step_select_tree <- function(x, ...) {
  tibble(
    name = c("top_p", "threshold", "cutoff", "cost_complexity", "tree_depth", "min_n"),
    call_info = list(
      list(pkg = "colino", fun = "top_p"),
      list(pkg = "dials", fun = "threshold", range = c(0, 1)),
      list(pkg = "colino", fun = "cutoff"),
      list(pkg = "dials", fun = "cost_complexity", range = c(-10, -1),
           trans = scales::log10_trans()),
      list(pkg = "dials", fun = "tree_depth", range = c(1L, 15L)),
      list(pkg = "dials", fun = "min_n", range = c(2L, 40L))
    ),
    source = "recipe",
    component = "step_select_tree",
    component_id = x$id
  )
}

#' @rdname required_pkgs.colino
#' @export
required_pkgs.step_select_tree <- function(x, ...) {
  c("colino")
}
