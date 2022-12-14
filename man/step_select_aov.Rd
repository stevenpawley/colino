% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/step_select_aov.R
\name{step_select_aov}
\alias{step_select_aov}
\alias{tidy.step_select_aov}
\title{Filter Categorical Predictors using the ANOVA F-Test}
\usage{
step_select_aov(
  recipe,
  ...,
  outcome,
  role = "predictor",
  trained = FALSE,
  top_p = NA,
  threshold = NA,
  cutoff = NA,
  exclude = NULL,
  skip = FALSE,
  id = recipes::rand_id("select_aov")
)

\method{tidy}{step_select_aov}(x, ...)
}
\arguments{
\item{recipe}{A recipe object. The step will be added to the sequence of
operations for this recipe.}

\item{...}{One or more selector functions to choose which predictors are
affected by the step. See [selections()] for more details. For the `tidy`
method, these are not currently used.}

\item{outcome}{A single character string that specifies a single numeric
variable.}

\item{role}{For model terms created by this step, what analysis role should
they be assigned? By default, the function assumes that resulting distances
will be used as predictors in a model.}

\item{trained}{A logical to indicate if the quantities for preprocessing have
been estimated.}

\item{top_p}{An integer that will be used to select the `top_p` predictors
with the smallest p-values. A value of `NA` implies that this criterion
will be ignored.}

\item{threshold}{A numeric value between 0 and 1 representing the percentile
of best scoring features to select. For example `threshold = 0.9` will
retain only predictors with scores in the top 90th percentile and a smaller
threshold will select more features. Note that `top_p` and `threshold` are
mutually exclusive but either can be used in conjunction with `cutoff` to
select the top-ranked features and those that are smaller than the cutoff
value.}

\item{cutoff}{A numeric value, in -log10(p-value) units, where predictors
with _larger_ than the cutoff will be retained. A value of `NA` implies
that this criterion will be ignored.}

\item{exclude}{A character vector of predictor names that will be removed
from the data. This will be set when `prep()` is used on the recipe and
should not be set by the user.}

\item{skip}{A logical. Should the step be skipped when the recipe is baked by
bake.recipe()? While all operations are baked when prep.recipe() is run,
some operations may not be able to be conducted on new data (e.g.
processing the outcome variable(s)). Care should be taken when using skip =
TRUE as it may affect the computations for subsequent operations.}

\item{id}{A character string that is unique to this step to identify it.}

\item{x}{A `step_select_aov` object.}
}
\value{
An updated version of `recipe` with the new step added to the
 sequence of existing steps (if any). For the `tidy` method, a tibble with a
 `terms` column for which predictors were removed.
}
\description{
`step_select_aov` creates a *specification* of a recipe step that will filter
predictors using their relationship with a numerical outcome as measured
using an ANOVA F-test.
}
\details{
The recipe will stop if both `top_p`, `threshold` or `cutoff` are left
unspecified. If both are used, they are combined via 'or'.
}
\examples{
data(ames, package = "modeldata")

rec <-
  recipe(Sale_Price ~ ., data = ames) \%>\%
  step_select_aov(
    all_nominal(),
    -all_outcomes(),
    outcome = "Sale_Price",
    top_p = 1,
    cutoff = -log10(0.01)
  ) \%>\%
  prep()

rec \%>\%
  juice(all_nominal()) \%>\%
  names()

tidy(rec, number = 1)
}
\concept{preprocessing}
\concept{supervised_filter}
\keyword{datagen}
