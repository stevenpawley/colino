#' Information gain feature selection step
#'
#' `step_select_infgain` creates a *specification* of a recipe step that selects a
#' subset of predictors based on the scores of the information gain algorithm.
#' This step requires the FSelectorRcpp package to be installed. The top
#' `top_p` scoring features, or features whose scores occur in the top
#' percentile `threshold` will be retained as new predictors.
#'
#' @inheritParams step_select_aov
#' @inherit step_select_aov return
#' @param role Not used by this step since no new variables are created.
#' @param type A character string specifying the information gain method to use.
#'   One of "infogain", "gainratio", "symuncert". The default is 'infogain'.
#' @param outcome A character string with the name of the response variable to
#'   use to evaluate information gain value against the predictors.
#' @param type The entropy measure. One of c("infogain", "gainratio",
#'   "symuncert"). The default is 'infogain'.
#' @param threads An integer specifying the number of threads to use for
#'   processing. The default = 0 uses all available threads.
#' @param nbins An integer specifying the number of bins for discretization.
#'   Only used if the outcome of a continuous variable for regression. The
#'   default is 'nbins = 5'.
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
#' The recipe will stop if both `top_p`, `threshold` and `cutoff` are left
#' unspecified.
#'
#' @examples
#' library(recipes)
#'
#' data(cells, package = "modeldata")
#'
#' rec <-
#'  recipe(class ~ ., data = cells[, -1]) %>%
#'  step_select_infgain(
#'    all_predictors(),
#'    outcome = "class",
#'    threshold = 0.9,
#'    id = "infgain"
#'  )
#'
#' prepped <- prep(rec)
#'
#' new_data <- juice(prepped)
#' prepped
step_select_infgain <- function(
  recipe, ...,
  outcome = NULL,
  role = NA,
  trained = FALSE,
  top_p = NA,
  threshold = NA,
  cutoff = NA,
  type = "infogain",
  nbins = 5,
  threads = 1,
  exclude = NULL,
  scores = NULL,
  skip = FALSE,
  id = recipes::rand_id("select_infgain")) {

  recipes::recipes_pkg_check("FSelectorRcpp")

  terms <- recipes::ellipse_check(...)

  recipes::add_step(
    recipe,
    step_select_infgain_new(
      terms = terms,
      trained = trained,
      outcome = outcome,
      role = role,
      top_p = top_p,
      threshold = threshold,
      cutoff = cutoff,
      type = type,
      threads = threads,
      nbins = nbins,
      exclude = exclude,
      scores = scores,
      skip = skip,
      id = id
    )
  )
}


# wrapper around 'step' function that sets the class of new step objects
step_select_infgain_new <- function(terms, role, trained, outcome, top_p,
                                    threshold, cutoff, type, threads, nbins,
                                    exclude, scores, skip, id) {
  recipes::step(
    subclass = "select_infgain",
    terms = terms,
    role = role,
    trained = trained,
    outcome = outcome,
    top_p = top_p,
    threshold = threshold,
    cutoff = cutoff,
    type = type,
    threads = threads,
    nbins = nbins,
    exclude = exclude,
    scores = scores,
    skip = skip,
    id = id
  )
}


#' @export
prep.step_select_infgain <- function(x, training, info = NULL, ...) {
  # extract response and predictor names
  x_names <- recipes::recipes_eval_select(x$terms, training, info)
  y_name <- recipes::recipes_eval_select(x$outcome, training, info)
  y_name <- y_name[1]

  # check criteria
  check_criteria(x$top_p, x$threshold, match.call())
  check_zero_one(x$threshold)
  x$top_p <- check_top_p(x$top_p, length(x_names))

  # information gain
  if (length(x_names) > 0) {

    f <- stats::as.formula(paste(y_name, "~", paste0(x_names, collapse = " + ")))
    model_mode <- check_outcome(training[[y_name]])
    equal <- model_mode == "regression"

    ig_call <- rlang::call2(
      .fn = "information_gain",
      .ns = "FSelectorRcpp",
      formula = f,
      data = rlang::quo(training),
      type = x$type,
      threads = x$threads,
      discIntegers = TRUE,
      equal = equal,
      nbins = x$nbins
    )

    res <- rlang::eval_tidy(ig_call)
    res <- as_tibble(res)
    res <- rlang::set_names(res, c("variable", "score"))
    res$score <- rlang::set_names(res$score, res$variable)

    exclude <-
      dual_filter(res$score, x$top_p, x$threshold, x$cutoff, maximize = TRUE)

  } else {
    exclude <- character()
  }

  step_select_infgain_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    outcome = y_name,
    top_p = x$top_p,
    threshold = x$threshold,
    cutoff = x$cutoff,
    type = x$type,
    threads = x$threads,
    nbins = x$nbins,
    exclude = exclude,
    scores = res,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_select_infgain <- function(object, new_data, ...) {
  if (length(object$exclude > 0)) {
    new_data <- new_data[, !(colnames(new_data) %in% object$exclude)]
  }
  as_tibble(new_data)
}

#' @export
print.step_select_infgain <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Information Gain feature selection")

    if (recipes::is_trained(x)) {
      n <- length(x$exclude)
      cat(paste0(" (", n, " excluded)"))
    }
    cat("\n")

    invisible(x)
  }

#' @rdname step_select_infgain
#' @param x A `step_select_infgain` object.
#' @param type A character with either 'terms' (the default) to return a
#'   tibble containing the variables that have been removed by the filter step,
#'   or 'scores' to return the scores for each variable.
#' @export
tidy.step_select_infgain <- function(x, type = "terms", ...) {
  tidy_filter_step(x, type)
}

#' @export
tunable.step_select_infgain <- function(x, ...) {
  tibble::tibble(
    name = c("top_p", "entropy", "threshold", "cutoff"),
    call_info = list(
      list(pkg = "colino", fun = "top_p"),
      list(pkg = "colino", fun = "entropy", values = values_entropy),
      list(pkg = "dials", fun = "threshold", range = c(0, 1)),
      list(pkg = "colino", fun = "cutoff")
    ),
    source = "recipe",
    component = "step_select_infgain",
    component_id = x$id
  )
}

#' @rdname required_pkgs.colino
#' @export
required_pkgs.step_select_infgain <- function(x, ...) {
  c("colino", "FSelectorRcpp")
}
