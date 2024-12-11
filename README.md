# colino

<!-- badges: start -->
[![R-CMD-check](https://github.com/stevenpawley/colino/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stevenpawley/colino/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/stevenpawley/colino/graph/badge.svg)](https://app.codecov.io/gh/stevenpawley/colino)
<!-- badges: end -->

The goal of colino is to provide supervised feature selection steps to be used
with the tidymodels recipes package. The overall focus of the package is on 
filter-based feature selection methods. Permutation score methods that use
a model can be considered a special case of filter approaches.

Note - colino is the new package name and replaces the preliminary
'recipeselectors' name. Colino will be submitted to CRAN once some additional
steps and documentation have been finalized.

## Installation

``` r
devtools::install_github("stevenpawley/colino")
```

## Feature Selection Methods

The following feature selection methods are implemented:

- `step_select_infgain` provides Information Gain feature selection. This step
requires the `FSelectorRcpp` package to be installed and can be used for both
classification and regression problems. For regression, the target variable is
discretized using equal frequency binning.

- `step_select_mrmr` provides maximum Relevancy Minimum Redundancy feature
selection. This step requires the `praznik` package to be installed. This step
can be used for classification and regression problems. Similar to information
gain, binning is used when the target variable is continuous.

- `step_select_roc` provides ROC-based feature selection based on each
predictors' relationship with the response outcomes measured using a Receiver
Operating Characteristic curve.

- `step_select_xtab` provides feature selection of categorical predictors using
statistical association for numeric response outcomes.

- `step_select_aov` provides feature selection of categorical predictors using
the ANOVA F-test for numeric response outcomes.

- `step_select_vip` provides model-based selection using feature importance
scores or coefficients. This method allows a `parsnip` model specification to be
used to select a subset of features based on the models' feature importances or
coefficients. See below for details.

- `step_select_boruta` provides a Boruta feature selection step. This step can
be used for classification and regression problems.

- `step_select_carscore` provides a CAR score feature selection step for
regression models. This step requires the `care` package to be installed.

- `step_select_relief` provides a Relief-based feature selection step for
classification and regression models. This step requires the `FSelectorRcpp`
package to be installed.

- `step_select_forests`, `step_select_tree`, and `step_select_linear` provide
model-based methods of selecting a subset of features based on the model's
feature importance scores or coefficients.

- `step_select_fcbf` provides the Fast Correlation Based Filter method of Yu and
Liu, 2003: Feature Selection for High-Dimensional Data: A Fast Correlation-Based
Filter Solution. This step is implemented in the Bioconductor package 'FCBF'
which can be installed using `BiocManager::install("FCBF")`.

## Feature Selection Criteria

Three parameters are used to filter features within the `step_select_` functions
in colino:

- `top_p` can be used to select the number of best scoring features to retain. 
This is nice because it is intuitive, but it suffers from the issue that you
do not always know how many features are present in your recipe, if you have
added/removed features in preceding recipe steps.

- `threshold` can be used to select the percentile of best-scoring features. For
example `threshold = 0.9` will retain only predictors with scores in the
top 90th percentile and a smaller threshold value will select more features.

- `cutoff` is a new argument that can use used to select features based on their
absolute feature scores. For example, if a `step_select_` method is based on
the p-values of features, then `cutoff` can be used to threshold the features
based on their p-value units. This requires knowledge of the domain space of
those values for any particular method.

Note that `top_p` and `threshold` are mutually exclusive but either can be used
in conjunction with `cutoff` to select the top-ranked features and those
that have filter scores that meet the cutoff threshold. For example, you can
require at least three features to be included by using `top_n = 3` but also
include any other features that meet the cutoff criteria, e.g., cutoff = 0.01 if
a method uses p-value units.

Most `step_select_` steps have `top_p`, `threshold` and `cutoff` available but
a few methods such as Boruta and FCBF do not rank the features, but only provide
a list of rejected features. These methods typically only have none of these
arguments, or only `cutoff`.

## Notes

The `step_select_vip` is designed to work with the `parsnip` package and
requires a base model specification that provides a method of ranking the
importance of features, such as feature importance scores or coefficients, with
one score per feature. The base model is specified in the step using the `model`
parameter.

Although `step_select_vip` allows a diverse range of models to be used as the
ranking algorithm, and potentially allows new models to be implemented, a
limitation is that the hyperparameters of the ranking model cannot be tuned. As
an alternative, `step_select_linear`, `step_select_tree` and
`step_select_forests` provide steps specific to these types of models where the
hyperparameters of ranking model can be tuned using the same tuning arguments as
`parsnip`.

The parsnip package does not currently contain a method of pulling feature
importance scores from models that support them. The `colino` package provides a
generic function `pull_importances` for this purpose that accepts a fitted
parsnip model, and returns a tibble with two columns 'feature' and 'importance':

```
model <- boost_tree(mode = "classification") %>%
  set_engine("xgboost")

model_fit <- model %>% 
  fit(Species ~., iris)

pull_importances(model_fit)
```

Most of the models and 'engines' that provide feature importances are
implemented. In addition, `h2o` models are supported using the `agua` package.
Use `methods(pull_importances)` to list models that are currently implemented.
If need to pull the feature importance scores from a model that is not currently
supported in this package, then you can add a class to the pull_importances
generic function which returns a two-column tibble:

```
pull_importances._ranger <- function(object, scaled = FALSE, ...) {
  scores <- ranger::importance(object$fit)

  # create a tibble with 'feature' and 'importance' columns
  scores <- tibble::tibble(
    feature = names(scores),
    importance = as.numeric(scores)
  )

  # optionally rescale the importance scores
  if (scaled)
    scores$importance <- scales::rescale(scores$importance)
  scores
}
```

An example of using the step_importance function:

```
library(parsnip)
library(recipes)
library(magrittr)

# load the example iris dataset
data(iris)

# define a base model to use for feature importances
base_model <- rand_forest(mode = "classification") %>%
  set_engine("ranger", importance = "permutation")

# create a preprocessing recipe
rec <- iris %>%
recipe(Species ~ .) %>%
step_select_vip(all_predictors(), model = base_model, top_p = 2,
                outcome = "Species")

prepped <- prep(rec)

# create a model specification
clf <- decision_tree(mode = "classification") %>%
set_engine("rpart")

clf_fitted <- clf %>%
  fit(Species ~ ., juice(prepped))
```
