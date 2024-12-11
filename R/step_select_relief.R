#' Feature selection step using the Relief algorithm
#'
#' Relief-based algorithms use nearest neighbors of randomly sampled
#' observations (without replacement) to derive feature weights/scores that
#' describe the relevance of each feature to the target variable. The feature
#' weights represent the differences between the normalized feature values from
#' each randomly sampled observation and a neighboring observation. If the
#' neighboring observation's class is the same as the sampled observation
#' (termed a 'hit') but the feature values are different, then this reduces the
#' score on the basis that widely varying feature values for the same class are
#' not desirable. Conversely, if a neighboring observation's class is different
#' from the sampled observation (termed a 'miss') and the feature values are
#' different, then this increases the score on the basis that observations of
#' different classes are widely separated by their feature values. The feature
#' weights / scores range from -1 (worst) to +1 (best).
#'
#' `step_select_relief` creates a *specification* of a recipe step that selects
#' a subset of predictors based on the scores of the relief algorithm. This step
#' requires the FSinR package to be installed. The top `top_p` scoring features,
#' or features whose scores occur in the top percentile `threshold` will be
#' retained as new predictors.
#'
#' @inheritParams step_select_aov
#' @inherit step_select_aov return
#' @param role Not used by this step since no new variables are created.
#' @param outcome A character string with the name of the response variable to
#'   use to evaluate information gain value against the predictors.
#' @param neighbors An integer with the number of neighbors for find for each
#'   sampled instance. Default is 5.
#' @param sample_size An integer with the number of instances to sample. Default
#'   is 10.
#' @param scores A tibble with 'variable' and 'scores' columns containing the
#'   names of the variables and their information gain scores. This parameter is
#'   only produced after the recipe has been trained.
#'
#' @export
#' @keywords datagen
#' @concept preprocessing
#' @concept supervised_filter
#' @export
#' @details
#'
#' The recipe will stop if all of `top_p`, `threshold` and `cutoff` are left
#' unspecified.
#'
#' @examples
#' \dontrun{
#' library(recipes)
#'
#' data(cells, package = "modeldata")
#'
#' rec <- recipe(class ~ ., data = cells[, -1]) %>%
#'   step_select_relief(
#'     all_predictors(),
#'     outcome = "class",
#'     top_p = 10
#'   )
#'
#'   prepped <- prep(rec)
#'   new_data <- bake(prepped, new_data = NULL)
#'   prepped
#' }
step_select_relief <- function(
    recipe, ...,
    outcome = NULL,
    role = NA,
    trained = FALSE,
    top_p = NA,
    threshold = NA,
    cutoff = NA,
    neighbors = 5,
    sample_size = 10,
    exclude = NULL,
    scores = NULL,
    skip = FALSE,
    id = recipes::rand_id("select_relief")) {

  recipes::recipes_pkg_check("FSinR")

  if (neighbors <= 0)
    rlang::abort("`neighbors` must be greater than zero")

  if (sample_size <= 0)
    rlang::abort("'sample_size' must be greater than zero")

  terms <- recipes::ellipse_check(...)

  recipes::add_step(
    recipe,
    step_select_relief_new(
      terms = terms,
      trained = trained,
      outcome = outcome,
      role = role,
      top_p = top_p,
      threshold = threshold,
      cutoff = cutoff,
      neighbors = neighbors,
      sample_size = sample_size,
      exclude = exclude,
      scores = scores,
      skip = skip,
      id = id
    )
  )
}


# wrapper around 'step' function that sets the class of new step objects
step_select_relief_new <-
  function(terms, role, trained, outcome, top_p, threshold, cutoff, neighbors,
           sample_size, exclude, scores, skip, id) {
  recipes::step(
    subclass = "select_relief",
    terms = terms,
    role = role,
    trained = trained,
    outcome = outcome,
    top_p = top_p,
    threshold = threshold,
    cutoff = cutoff,
    neighbors = neighbors,
    sample_size = sample_size,
    exclude = exclude,
    scores = scores,
    skip = skip,
    id = id
  )
}


#' @export
prep.step_select_relief <- function(x, training, info = NULL, ...) {
  x_names <- recipes::recipes_eval_select(x$terms, training, info)
  y_name <- recipes::recipes_eval_select(x$outcome, training, info)
  y_name <- y_name[1]

  # check criteria
  check_criteria(x$top_p, x$threshold, match.call())
  check_zero_one(x$threshold)
  x$top_p <- check_top_p(x$top_p, length(x_names))

  # feature selection
  if (length(x_names) > 0) {
    call_func <- rlang::call2(
      .fn = "relief",
      .ns = "FSelectorRcpp",
      x = rlang::quo(as.data.frame(training[, x_names])),
      y = rlang::quo(training[[y_name]]),
      neighboursCount = x$neighbors,
      sampleSize = x$sample_size
    )
    res <- rlang::eval_tidy(call_func)
    res <- as_tibble(res)
    res <- rlang::set_names(res, c("variable", "score"))
    res$score <- rlang::set_names(res$score, res$variable)
    res <- res[order(res$score, decreasing = TRUE), ]

    exclude <-
      dual_filter(res$score, x$top_p, x$threshold, x$cutoff, maximize = TRUE)

  } else {
    exclude <- character()
  }

  step_select_relief_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    outcome = y_name,
    top_p = x$top_p,
    threshold = x$threshold,
    cutoff = x$cutoff,
    neighbors = x$neighbors,
    sample_size = x$sample_size,
    exclude = exclude,
    scores = res,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_select_relief <- function(object, new_data, ...) {
  if (length(object$exclude > 0)) {
    new_data <- new_data[, !(colnames(new_data) %in% object$exclude)]
  }
  as_tibble(new_data)
}

#' @export
print.step_select_relief <- function(x, width = max(20, options()$width - 30), ...) {
  cat("Relief feature selection")

  if (recipes::is_trained(x)) {
    n <- length(x$exclude)
    cat(paste0(" (", n, " excluded)"))
  }
  cat("\n")

  invisible(x)
}

#' @rdname step_select_relief
#' @param x A `step_select_relief` object.
#' @export
tidy.step_select_relief <- function(x, ...) {
  if (recipes::is_trained(x)) {
    res <- tibble(terms = x$exclude)
  } else {
    term_names <- recipes::sel2char(x$terms)
    res <- tibble(terms = rlang::na_chr)
  }
  res$id <- x$id
  res
}

#' @export
tunable.step_select_relief <- function(x, ...) {
  tibble::tibble(
    name = c("top_p", "threshold", "cutoff"),
    call_info = list(
      list(pkg = "colino", fun = "top_p"),
      list(pkg = "dials", fun = "threshold", range = c(0, 1)),
      list(pkg = "colino", fun = "cutoff")
    ),
    source = "recipe",
    component = "step_select_relief",
    component_id = x$id
  )
}

#' @rdname required_pkgs.colino
#' @export
required_pkgs.step_select_relief <- function(x, ...) {
  c("colino", "FSinR")
}
