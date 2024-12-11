#' Feature selection step using the CAR score algorithm
#'
#' `step_select_carscore` creates a *specification* of a recipe step that
#' selects a subset of predictors as part of a regression model based on the
#' scores of the CAR score algorithm. This step requires the `care` package to be
#' installed. The top `top_p` scoring features, or features whose scores occur
#' in the top percentile `threshold` will be retained as new predictors.
#'
#' @inheritParams step_select_aov
#' @inherit step_select_aov return
#' @param role Not used by this step since no new variables are created.
#' @param lambda The correlation shrinkage intensity (range 0-1).
#' @param diagonal For diagonal = FALSE (the default) CAR scores are computed;
#'   otherwise with diagonal = TRUE marginal correlations.
#' @param outcome A character string with the name of the response variable.
#'   This must refer to a numeric feature for regression.
#' @param scores A tibble with 'variable' and 'scores' columns containing the
#'   names of the variables and the absolute values of the calculated CAR
#'   scores. This parameter is only produced after the recipe has been trained.
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
#' library(recipes)
#'
#' data(car_prices, package = "modeldata")
#'
#' rec <-
#'  recipe(Price ~ ., data = car_prices) %>%
#'  step_select_carscore(
#'    all_predictors(),
#'    outcome = "Price",
#'    top_p = 5,
#'    cutoff = 0.7
#'  )
#'
#' prepped <- prep(rec)
#'
#' new_data <- bake(prepped, new_data = NULL)
#' prepped
step_select_carscore <- function(
  recipe, ...,
  outcome = NULL,
  role = NA,
  trained = FALSE,
  top_p = NA,
  threshold = NA,
  cutoff = NA,
  lambda = NA,
  diagonal = FALSE,
  exclude = NULL,
  scores = NULL,
  skip = FALSE,
  id = recipes::rand_id("select_carscore")) {

  recipes::recipes_pkg_check("care")

  terms <- recipes::ellipse_check(...)

  recipes::add_step(
    recipe,
    step_select_carscore_new(
      terms = terms,
      trained = trained,
      outcome = outcome,
      role = role,
      top_p = top_p,
      threshold = threshold,
      cutoff = cutoff,
      lambda = lambda,
      diagonal = diagonal,
      exclude = exclude,
      scores = scores,
      skip = skip,
      id = id
    )
  )
}


# wrapper around 'step' function that sets the class of new step objects
step_select_carscore_new <-
  function(terms, role, trained, outcome, top_p, threshold, cutoff, lambda,
           diagonal, exclude, scores, skip, id) {
    recipes::step(
      subclass = "select_carscore",
      terms = terms,
      role = role,
      trained = trained,
      outcome = outcome,
      top_p = top_p,
      threshold = threshold,
      cutoff = cutoff,
      lambda = lambda,
      diagonal = diagonal,
      exclude = exclude,
      scores = scores,
      skip = skip,
      id = id
    )
  }


#' @export
prep.step_select_carscore <- function(x, training, info = NULL, ...) {

  # extract response and predictor names
  x_names <- recipes::recipes_eval_select(x$terms, training, info)
  y_name <- recipes::recipes_eval_select(x$outcome, training, info)
  y_name <- y_name[1]

  # check criteria
  recipes::check_type(training[, y_name], quant = TRUE)
  check_criteria(x$top_p, x$threshold, match.call())
  check_zero_one(x$threshold)
  x$top_p <- check_top_p(x$top_p, length(x_names))

  # information gain
  if (length(x_names) > 0) {

    args <- list()

    if (!is.na(x$lambda))
      args$lambda <- x$lambda

    call <- rlang::call2(
      .fn = "carscore",
      .ns = "care",
      Xtrain = training[, x_names],
      Ytrain = training[, y_name],
      diagonal = x$diagonal,
      !!!args
    )

    res <- rlang::eval_tidy(call)

    res <- tibble(
      variable = names(res),
      score = abs(res)
    )

    exclude <-
      dual_filter(res$score, x$top_p, x$threshold, x$cutoff, maximize = TRUE)

  } else {
    exclude <- character()
  }

  step_select_carscore_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    outcome = y_name,
    top_p = x$top_p,
    threshold = x$threshold,
    cutoff = x$cutoff,
    lambda = x$lambda,
    diagonal = x$diagonal,
    exclude = exclude,
    scores = res,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_select_carscore <- function(object, new_data, ...) {
  if (length(object$exclude > 0)) {
    new_data <- new_data[, !(colnames(new_data) %in% object$exclude)]
  }
  as_tibble(new_data)
}

#' @export
print.step_select_carscore <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Carscore feature selection")

    if (recipes::is_trained(x)) {
      n <- length(x$exclude)
      cat(paste0(" (", n, " excluded)"))
    }
    cat("\n")

    invisible(x)
  }

#' @rdname step_select_carscore
#' @param x A `step_select_carscore` object.
#' @param type A character with either 'terms' (the default) to return a
#'   tibble containing the variables that have been removed by the filter step,
#'   or 'scores' to return the scores for each variable.
#' @export
tidy.step_select_carscore <- function(x, type = "terms", ...) {
  tidy_filter_step(x, type)
}

#' @export
tunable.step_select_carscore <- function(x, ...) {
  tibble::tibble(
    name = c("top_p", "threshold", "cutoff"),
    call_info = list(
      list(pkg = "colino", fun = "top_p"),
      list(pkg = "dials", fun = "threshold", range = c(0, 1)),
      list(pkg = "colino", fun = "cutoff")
    ),
    source = "recipe",
    component = "step_select_carscore",
    component_id = x$id
  )
}

#' @rdname required_pkgs.colino
#' @export
required_pkgs.step_select_carscore <- function(x, ...) {
  c("colino", "care")
}
