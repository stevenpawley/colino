#' Parameter functions for feature selection recipes
#'
#' Feature selection recipes allow the top-performing features to be selected
#' using three parameters. `top_p` is for specifying the number of the
#' top-performing features.
#'
#' @param range A two-element vector holding the _defaults_ for the smallest and
#'   largest possible values, respectively.
#' @param trans A `trans` object from the `scales` package, such as
#'   `scales::log10_trans()` or `scales::reciprocal_trans()`. If not provided,
#'   the default is used which matches the units used in `range`. If no
#'   transformation, `NULL`.
#'
#' @return A function with classes "quant_param" and "param"
#' @export
#'
#' @examples
#' top_p(c(3, 10))
top_p <- function(range = c(1L, 4L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(top_p = "# Selected Predictors"),
    finalize = dials::get_p
  )
}

#' Parameter functions for feature selection recipes
#'
#' Feature selection recipes allow the top-performing features to be selected
#' using three parameters. `cutoff` is for selecting features using the absolute
#' value in the filter methods scores.
#'
#' @param range A two-element vector holding the _defaults_ for the smallest and
#'   largest possible values, respectively.
#' @param trans A `trans` object from the `scales` package, such as
#'   `scales::log10_trans()` or `scales::reciprocal_trans()`. If not provided,
#'   the default is used which matches the units used in `range`. If no
#'   transformation, `NULL`.
#'
#' @return A function with classes "quant_param" and "param"
#' @export
#'
#' @examples
#' cutoff(c(3.5, 15))
cutoff <- function(range = c(dials::unknown(), dials::unknown()), trans = NULL) {
  dials::new_quant_param(
    type = "numeric",
    range = range,
    inclusive = c(FALSE, FALSE),
    trans = trans,
    label = c(cutoff = "Absolute cutoff threshold for the feature scores")
  )
}

values_entropy <- c("infogain", "gainratio", "symuncert")


#' Parameter functions for feature selection recipes
#'
#' Entropy-based feature selection methods can be applied using several methods
#' to calculate the entropy formula. `entropy` is for specifying the type of
#' entropy-based filter that is used.
#'
#' @param values A character string of possible values. See `values_entropy` for
#'   possible values.
#'
#' @return A function with classes "qual_param" and "param"
#' @export
#'
#' @examples
#' entropy('infogain')
entropy <- function(values = values_entropy) {
  dials::new_qual_param(
    type = "character",
    values = values,
    default = "infogain",
    label = c(entropy = "Method used for entropy-based feature selection"),
    finalize = NULL
  )
}
