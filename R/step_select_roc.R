#' Filter Numeric Predictors using ROC Curve
#'
#' `step_select_roc` creates a *specification* of a recipe step that will
#'  filter predictors using their relationship with the outcome as measured
#'  using a Receiver Operating Characteristic curve.
#'
#' @inheritParams step_select_aov
#' @inherit step_select_aov return
#' @param outcome A single character string that specifies a single categorical
#'  variable to be used as the class.
#' @param role For model terms created by this step, what analysis role should
#'  they be assigned?. By default, the function assumes that resulting distances
#'  will be used as predictors in a model.
#'
#' @keywords datagen
#' @concept preprocessing
#' @concept supervised_filter
#' @export
#' @details
#'
#' The recipe will stop if all of `top_p`, `threshold` and `cutoff` are left
#' unspecified.
#'
#' The ROC AUC will be set to be 1 - AUC if the value is less than 0.50.
#'
#' @examples
#' data(cells, package = "modeldata")
#'
#' rec <-
#'   recipe(class ~ ., data = cells[, -1]) %>%
#'   step_select_roc(all_predictors(), outcome = "class", top_p = 10, cutoff = 0.9) %>%
#'   prep()
#'
#' rec %>% bake(all_predictors(), new_data = NULL) %>% names()
#'
#' # Use ROC values to select but always keep at least one:
#' rec <-
#'   recipe(class ~ ., data = cells[, -1]) %>%
#'   step_select_roc(
#'     all_predictors(),
#'     outcome = "class",
#'     top_p = 1,
#'     cutoff = 0.99
#'   ) %>%
#'   prep()
#'
#' rec %>% juice(all_predictors()) %>% names()
step_select_roc <-
  function(recipe, ..., outcome, role = "predictor", trained = FALSE,
           threshold = NA, top_p = NA, cutoff = NA, exclude = NULL,
           skip = FALSE, id = recipes::rand_id("select_roc")) {
  recipes::add_step(
    recipe,
    step_select_roc_new(
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

step_select_roc_new <-
  function(terms, outcome, role, trained, top_p, threshold, cutoff, exclude,
           skip, id) {
    recipes::step(
      subclass = "select_roc",
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

roc_calc <- function(x, y) {
  suppressMessages(
    suppressWarnings(
      {
        if (length(levels(y)) == 2) {
          res <- try(pROC::roc(y, x, direction = "auto"),
                     silent = TRUE)
        } else {
          res <- try(pROC::multiclass.roc(y, x, direction = "auto"),
                     silent = TRUE)
        }
      }
    )
  )

  if (inherits(res, "try-error")) {
    res <- NA_real_
  } else {
    res <- unname(pROC::auc(res))
  }
  res
}

#' @export
prep.step_select_roc <- function(x, training, info = NULL, ...) {
  y_name <- recipes::recipes_eval_select(x$outcome, training, info)
  y_name <- x$outcome[1]
  recipes::check_type(training[, y_name], quant = FALSE)
  x_names <- recipes::recipes_eval_select(x$terms, training, info)

  if (length(x_names) > 0) {

    recipes::check_type(training[, x_names])

    # check criteria
    check_criteria(x$top_p, x$threshold, match.call())
    check_zero_one(x$threshold)
    x$top_p <- check_top_p(x$top_p, length(x_names))

    # filter
    scores <- purrr::map_dbl(training[, x_names], ~ roc_calc(.x, training[[y_name]]))
    exclude_chr <- dual_filter(scores, x$top_p, x$threshold, x$cutoff,
                               maximize = TRUE)
  } else {
    exclude_chr <- character()
  }

  step_select_roc_new(
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
bake.step_select_roc <- function(object, new_data, ...) {
  if (length(object$exclude) > 0) {
    new_data <- new_data %>%
      dplyr::select(-dplyr::one_of(object$exclude))
  }
  new_data
}

#' @export
print.step_select_roc <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("ROC curve feature selection")

    if (recipes::is_trained(x)) {
      n <- length(x$exclude)
      cat(paste0(" (", n, " excluded)"))
    }
    cat("\n")

    invisible(x)
  }

#' @rdname step_select_roc
#' @param x A `step_select_roc` object.
#' @export
tidy.step_select_roc <- function(x, ...) {
  tidy_filter_step(x, type = "terms")
}

#' @export
tunable.step_select_roc <- function(x, ...) {
  tibble::tibble(
    name = c("top_p", "threshold", "cutoff"),
    call_info = list(
      list(pkg = "colino", fun = "top_p"),
      list(pkg = "dials", fun = "threshold", range = c(0, 1)),
      list(pkg = "colino", fun = "cutoff")
    ),
    source = "recipe",
    component = "step_select_roc",
    component_id = x$id
  )
}

#' @rdname required_pkgs.colino
#' @export
required_pkgs.step_select_roc <- function(x, ...) {
  c("colino", "pROC")
}
