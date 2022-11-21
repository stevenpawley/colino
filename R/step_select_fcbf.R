#' Fast Correlation Based Filter for Feature Selection
#'
#' `step_select_fcbf` creates a *specification* of a recipe step that selects a
#' subset of  predictors using the FCBF algorithm. The number of features
#' retained depends on the `threshold` parameter: a lower threshold
#' selects more features.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step, e.g. all_numeric_predictors(). Any features not
#'   selected will be retained in the recipe.
#' @param threshold A numeric value between 0 and 1 representing the symmetrical
#'   uncertainty threshold used by the FCBF algorithm. Lower thresholds allow
#'   more features to be selected.
#' @param outcome A character string specifying the name of the response
#'   variable. Automatically inferred from the recipe (if possible) when not
#'   specified by the user.
#' @param cutpoint A numeric value between 0 and 1 representing the quantile at
#'   which to split numeric features into binary nominal features. e.g. 0.5 =
#'   median split. See details for more information on discretization
#' @param features_retained A tibble containing the features that were retained
#'   by the FCBF algorithm. This parameter is only produced after the recipe has
#'   been trained and should not be specified by the user
#' @param removals A tibble containing the features that were removed
#'   by the FCBF algorithm. This parameter is only produced after the recipe has
#'   been trained, and should not be specified by the user
#' @param role Not used for this step since new variables are not created.
#' @param trained A logical to indicate if the quantities for preprocessing have
#'   been estimated.
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   bake.recipe()? While all operations are baked when prep.recipe() is run,
#'   some operations may not be able to be conducted on new data (e.g.
#'   processing the outcome variable(s)). Care should be taken when using skip =
#'   TRUE as it may affect the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#'
#' @details
#' This function implements the fast correlation-based filter (FCBF)
#' algorithm as described in Yu & Liu (2003). FCBF selects features that
#' have high correlation to the outcome, and low correlation to other features.
#'
#' Symmetrical uncertainty (SU) is used to indicate the degree of correlation
#' between predictors and the outcome. A threshold value for SU must be
#' specified, and smaller thresholds values will result in more features being
#' selected by the algorithm. Appropriate thresholds are data-dependent, so
#' different threshold values may need to be explored. It is not possible to
#' specify an exact number of features that should be retained
#'
#' The algorithm requires categorical features, so continuous features are
#' discretized using a binary split (split at the median by default).
#' Discretization is only used within the feature selection algorithm,
#' selected features are then retained in their original continuous form for
#' further processing.
#'
#' The FCBF algorithm is implemented by the Bioconductor package 'FCBF', which
#' can be installed with BiocManager::install("FCBF")

#' @return Returns the recipe object, with step_select_fcbf added to the
#'   sequence of operations for this recipe.
#'
#' @references Yu, L. and Liu, H. (2003); Feature Selection for High-Dimensional
#'   Data A Fast Correlation Based Filter Solution, Proc. 20th Intl. Conf. Mach.
#'   Learn. (ICML-2003), Washington DC, 2003.
#'
#' @export
#' @importFrom recipes rand_id add_step recipes_pkg_check
#' @importFrom rlang enquos .data
#' @examples
#' library(recipes)
#' library(colino)
#'
#' # Load the example iris dataset
#' data("iris")
#'
#' # Create a preprocessing recipe including FCBF
#' my_recipe <- recipe(Species ~ ., data = iris) %>%
#'     step_select_fcbf(all_predictors(), threshold = 0.001)
#'
#' prepped <- prep(my_recipe, iris)
#'
#' new_data <- juice(prepped, iris)
#' prepped

step_select_fcbf <-
  function(recipe,
           ...,
           threshold = 0.025,
           outcome = NA,
           cutpoint = 0.5,
           features_retained = NA,
           removals = NULL,
           role = NA,
           trained = FALSE,
           skip = FALSE,
           id = rand_id("select_fcbf")) {
    # check for packages
    recipes_pkg_check(required_pkgs.step_select_fcbf())

    # check arguments
    if (is.na(threshold)) {
      rlang::abort("threshold must be a number between 0-1")
    }

    if (is.na(cutpoint)) {
      rlang::abort("cutpoint must be a number between 0-1")
    }

    if (!is.numeric(threshold) | threshold >= 1 | threshold <= 0) {
      rlang::abort("threshold must be a number between 0-1")
    }

    if (!is.numeric(cutpoint) | cutpoint >= 1 | cutpoint <= 0) {
      rlang::abort("cutpoint must be a number between 0-1")
    }

    add_step(
      recipe,
      step_select_fcbf_new(
        terms = enquos(...),
        threshold = threshold,
        outcome = outcome,
        cutpoint = cutpoint,
        features_retained = features_retained,
        role = role,
        trained = trained,
        removals = removals,
        skip = skip,
        id = id
      )
    )
  }

#' @importFrom recipes step
step_select_fcbf_new <-
  function(terms,
           threshold,
           outcome,
           cutpoint,
           features_retained,
           role,
           trained,
           removals,
           skip,
           id) {
    step(
      subclass = "select_fcbf",
      terms = terms,
      threshold = threshold,
      outcome = outcome,
      cutpoint = cutpoint,
      features_retained = features_retained,
      role = role,
      trained = trained,
      removals = removals,
      skip = skip,
      id = id
    )
  }
#' @importFrom recipes prep recipes_eval_select
#' @importFrom dplyr filter pull
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
prep.step_select_fcbf <- function(x, training, info = NULL, ...) {
  # find outcome column
  outcome_col <- get_outcome(x, training, info)

  # get predictor columns to be provided to FCBF algorithm
  # extract predictor columns selected by user or tidyselect functions
  preds_input <- recipes_eval_select(x$terms, training, info)

  # exclude any columns full of NA values (can break FCBF function)
  preds_fcbf <- remove_NA_cols(preds_input, training)

  # catch any issues that may break the FCBF code
  if (length(preds_fcbf) == 1) {
    # a message is not given by FCBF::fcbf when only 1 predictor is provided to the filter
    rlang::warn(
      paste(
        "Only one usable predictor was supplied to",
        "step_select_fcbf. FCBF may have unexpected results."
      )
    )

  } else if (length(preds_fcbf) == 0) {
    rlang::abort("No usable predictors were supplied to step_select_fcbf.")
  }

  # run FCBF
  fcbf_out <-
    FCBF_helper(
      preds = training[, preds_fcbf, drop = FALSE],
      outcome = training[, outcome_col, drop = TRUE],
      threshold = x$threshold,
      cutpoint = x$cutpoint
    )

  cols_selected <- preds_fcbf[fcbf_out$index]

  # specify which cols to remove from the training set
  remove_cols <- preds_input[!preds_input %in% cols_selected]

  # keep list of which predictors were retained (potentially useful for user)
  feats_retained <- info %>%
    filter(
      .data$role == 'predictor',
      !.data$variable %in% remove_cols
    )

  step_select_fcbf_new(
    terms = x$terms,
    threshold = x$threshold,
    outcome = outcome_col,
    cutpoint = x$cutpoint,
    features_retained = feats_retained,
    role = x$role,
    trained = TRUE,
    removals = remove_cols,
    skip = x$skip,
    id = x$id
  )
}

#' @importFrom recipes bake
#' @importFrom tibble as_tibble
#' @export
bake.step_select_fcbf <- function(object, new_data, ...) {
  if (length(object$removals) > 0) {
    new_data <- new_data[, !colnames(new_data) %in% object$removals]
  }

  as_tibble(new_data)
}

#' @export
#' @importFrom recipes print_step
print.step_select_fcbf <-
  function(x, width = max(20, options()$width - 36), ...) {
    if (x$trained) {
      title <- "FCBF retained : "

      print_step(x$features_retained$variable,
                 x$terms,
                 x$trained,
                 title,
                 width)

      title <- "FCBF removed: "
      print_step(x$removals, x$terms, x$trained, title, width)

    } else {
      print_step(untr_obj = x$terms,
                 title = "FCBF applied to features: ",
                 width = width)
    }

    invisible(x)
  }

#' S3 methods for tracking which additional packages are needed for steps.
#'
#' Recipe-adjacent packages always list themselves as a required package so that
#' the steps can function properly within parallel processing schemes.
#' @param x A recipe step
#' @return A character vector
#' @rdname required_pkgs.embed
#' @keywords internal
#' @export
required_pkgs.step_select_fcbf <- function(x, ...) {
  c("FCBF")
}

discretize_var <- function(numeric_feat, cutpoint) {
  # for an odd-length vector, median gets included in 'l' (low) group
  # NAs are ignored and will remain as NA in the discretized variable
  if (!is.numeric(numeric_feat)) {
    rlang::abort("Feature must be numeric to discretize")
  }

  cut <- stats::quantile(numeric_feat, cutpoint, na.rm = TRUE)

  results <- rep(NA, length(numeric_feat)) # initialize all as NA
  results[numeric_feat <= cut] <- 'l' # below cut as 'low'
  results[numeric_feat > cut] <- 'h' # set values above cut as 'high'

  return(as.factor(results))
}

FCBF_helper <- function(preds, outcome, threshold, cutpoint) {
  # Takes a set of predictors, does FCBF for feature selection, and
  # returns the names of the features to keep.
  preds <- preds %>%
    purrr::map_if(is.numeric, ~ discretize_var(.x, cutpoint = cutpoint)) %>%
    purrr::map_if(function(x) {!is.factor(x)}, ~ as.factor(.x)) %>%
    as_tibble()

  call <- rlang::call2(
    .fn = "fcbf",
    .ns = "FCBF",
    feature_table = preds,
    target_vector = outcome,
    minimum_su = threshold,
    verbose = FALSE,
    samples_in_rows = TRUE
  )

  quiet_call <- purrr::quietly(rlang::eval_tidy)
  res <- quiet_call(call)$result

  return(res)
}

remove_NA_cols <- function(pred_colnames, df) {
  # Takes a df and character vector of columns names. Columns full of NA are
  # removed from the character vector
  NAcols <- purrr::map_lgl(df[, pred_colnames], ~ all(is.na(.x)))

  if (sum(NAcols) > 0) {
    rlang::warn(paste(
      sum(NAcols),
      "features were full of NAs and removed prior to FCBF"
    ))
  }

  return(pred_colnames[!NAcols])
}
