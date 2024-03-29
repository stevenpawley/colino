% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/step_select_tree.R
\name{step_select_tree}
\alias{step_select_tree}
\alias{tidy.step_select_tree}
\title{Feature selection step using a decision tree importance scores}
\usage{
step_select_tree(
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
  id = recipes::rand_id("select_tree")
)

\method{tidy}{step_select_tree}(x, type = "terms", ...)
}
\arguments{
\item{recipe}{A recipe object. The step will be added to the sequence of
operations for this recipe.}

\item{...}{One or more selector functions to choose which variables are
affected by the step. See selections() for more details. For the tidy
method, these are not currently used.}

\item{outcome}{A character string with the name of the response variable to
use to calculate the feature importance scores.}

\item{role}{Not used by this step since no new variables are created.}

\item{trained}{A logical to indicate if the quantities for preprocessing have
been estimated.}

\item{engine}{A supported rand_forest engine that is supported by parsnip.
The default is "rpart".}

\item{cost_complexity}{A positive number for the the cost/complexity
parameter (a.k.a. Cp) used by CART models (specific engines only).}

\item{tree_depth}{An integer for maximum depth of the tree.}

\item{min_n}{An integer for the minimum number of data points in a node that
are required for the node to be split further.}

\item{top_p}{An integer with the number of best scoring features to select.}

\item{threshold}{A numeric value between 0 and 1 representing the percentile
of best scoring features to select. For example `threshold = 0.9` will
retain only predictors with scores in the top 90th percentile and a smaller
threshold will select more features. Note that `top_p` and `threshold` are
mutually exclusive but either can be used in conjunction with `cutoff` to
select the top-ranked features and those that have filter scores that are
larger than the cutoff value.}

\item{cutoff}{A numeric value where predictors with _larger_ absolute filter
scores than the cutoff will be retained. A value of `NA` implies that this
criterion will be ignored.}

\item{exclude}{A character vector of predictor names that will be removed
from the data. This will be set when `prep()` is used on the recipe and
should not be set by the user.}

\item{scores}{A tibble with 'variable' and 'scores' columns containing the
names of the variables and their feature importance scores. This parameter
is only produced after the recipe has been trained.}

\item{skip}{A logical. Should the step be skipped when the recipe is baked by
bake.recipe()? While all operations are baked when prep.recipe() is run,
some operations may not be able to be conducted on new data (e.g.
processing the outcome variable(s)). Care should be taken when using skip =
TRUE as it may affect the computations for subsequent operations.}

\item{id}{A character string that is unique to this step to identify it.}

\item{x}{A `step_select_tree` object.}

\item{type}{A character with either 'terms' (the default) to return a
tibble containing the variables that have been removed by the filter step,
or 'scores' to return the scores for each variable.}
}
\value{
a `step_select_tree` object.
}
\description{
`step_select_tree` creates a *specification* of a recipe step that selects a
subset of predictors based on the ranking of variable importance provided by
a `parsnip::decision_tree` supported model.
}
\details{
The recipe will stop if all of `top_p`, `threshold` and `cutoff` are left
unspecified.
}
\examples{
library(recipes)
library(parsnip)

# load the example cells dataset
data(cells, package = "modeldata")

# create a preprocessing recipe
rec <-
 recipe(class ~ ., data = cells[, -1]) \%>\%
 step_select_tree(all_predictors(), outcome = "class", top_p = 10)

prepped <- prep(rec)

preproc_data <- bake(prepped, new_data = NULL)
prepped
}
