#' Feature selection step using the magnitude of a linear models' coefficients
#'
#' `step_select_linear` creates a *specification* of a recipe step that selects
#' a subset of predictors based on the ranking of the magnitude of coefficients
#' provided by a `parsnip::linear_reg` or `parsnip::logistic_reg` model.
#'
#' @inheritParams step_select_aov
#' @inherit step_select_aov return
#' @param outcome A character string with the name of the response variable to
#'   use to calculate the feature importance scores.
#' @param role Not used by this step since no new variables are created.
#' @param engine A supported rand_forest engine that is supported by parsnip.
#'   The default is "glm".
#' @param penalty A non-negative number representing the total amount of
#'   regularization (specific engines only).
#' @param mixture A number between zero and one (inclusive) that is the
#'   proportion of L1 regularization (i.e. lasso) in the model. When mixture =
#'   1, it is a pure lasso model while mixture = 0 indicates that ridge
#'   regression is being used (specific engines only).
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
#'  step_select_linear(
#'    all_predictors(),
#'    outcome = "class",
#'    threshold = 0.9
#'  )
#'
#' prepped <- prep(rec)
#'
#' preproc_data <- bake(prepped, new_data = NULL)
#' prepped
step_select_linear <- function(
    recipe,
    ...,
    outcome = NULL,
    role = "predictor",
    trained = FALSE,
    engine = "glm",
    penalty = NULL,
    mixture = NULL,
    top_p = NA,
    threshold = NA,
    cutoff = NA,
    exclude = NULL,
    scores = NULL,
    skip = FALSE,
    id = recipes::rand_id("select_linear")) {

  engines <- c(
    parsnip::show_engines("linear_reg")$engine,
    parsnip::show_engines("logistic_reg")$engine
  )

  if (!engine %in% engines) {
    rlang::abort(
      paste("Engine argument should be one of", paste(engines, collapse = ", "))
    )
  }

  recipes::add_step(
    recipe,
    step_select_linear_new(
      terms = recipes::ellipse_check(...),
      trained = trained,
      outcome = outcome,
      role = role,
      engine = engine,
      penalty = penalty,
      mixture = mixture,
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
step_select_linear_new <- function(terms, role, trained, outcome, engine,
                                   top_p, threshold, cutoff, penalty, mixture,
                                   exclude, scores, skip, id) {
  recipes::step(
    subclass = "select_linear",
    terms = terms,
    role = role,
    trained = trained,
    outcome = outcome,
    engine = engine,
    penalty = penalty,
    mixture = mixture,
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
prep.step_select_linear <- function(x, training, info = NULL, ...) {

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
      penalty = x$penalty,
      mixture = x$mixture
    )

    if (model_mode == "classification") {
      if (length(levels(y)) == 2) {
        model_spec <-
          parsnip::make_call("logistic_reg", args = model_args, ns = "parsnip")
      } else {
        rlang::abort(
          "`step_select_linear` is only applicable to regression or binary classifications"
        )
      }

    } else if (model_mode == "regression") {
      model_spec <-
        parsnip::make_call("linear_reg", args = model_args, ns = "parsnip")
    }

    model_spec <-
      rlang::eval_tidy(model_spec) %>%
      parsnip::set_mode(model_mode) %>%
      parsnip::set_engine(x$engine)

    initial_model <- parsnip::fit_xy(model_spec, X, y)
    res <- pull_importances(initial_model)
    res$importance <- abs(res$importance)
    names(res) <- c("variable", "score")
    res$score <- rlang::set_names(res$score, res$variable)

    exclude <-
      dual_filter(res$score, x$top_p, x$threshold, x$cutoff, maximize = TRUE)

    missing_scores <- res[is.na(res$score), ][["variable"]]
    exclude <- c(exclude, missing_scores)

  } else {
    exclude <- character()
  }

  step_select_linear_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    outcome = y_name,
    engine = x$engine,
    penalty = x$penalty,
    mixture = x$mixture,
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
bake.step_select_linear <- function(object, new_data, ...) {
  if (length(object$exclude) > 0) {
    new_data <- new_data[, !colnames(new_data) %in% object$exclude]
  }

  as_tibble(new_data)
}

#' @export
print.step_select_linear <-
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

#' @rdname step_select_linear
#' @param x A `step_select_linear` object.
#' @param type A character with either 'terms' (the default) to return a
#'   tibble containing the variables that have been removed by the filter step,
#'   or 'scores' to return the scores for each variable.
#' @export
tidy.step_select_linear <- function(x, type = "terms", ...) {
  tidy_filter_step(x, type)
}

#' @export
tunable.step_select_linear <- function(x, ...) {
  tibble(
    name = c("top_p", "threshold", "cutoff", "penalty", "mixture"),
    call_info = list(
      list(pkg = "colino", fun = "top_p"),
      list(pkg = "dials", fun = "threshold", range = c(0, 1)),
      list(pkg = "colino", fun = "cutoff"),
      list(pkg = "dials", fun = "penalty", range = c(-10, 0),
           trans = scales::log10_trans()),
      list(pkg = "dials", fun = "mixture", range = c(0, 1))
    ),
    source = "recipe",
    component = "step_select_linear",
    component_id = x$id
  )
}

#' @rdname required_pkgs.colino
#' @export
required_pkgs.step_select_linear <- function(x, ...) {
  c("colino")
}
