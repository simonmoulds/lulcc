#' @include class-StatModels.R
NULL

#' Create a StatModels object
#'
#' Methods to create a \code{\link{StatModels-class}} object.
#'
#' TODO
#' 
#' @param models a list of predictive models
#' @param obs an ObservedMaps object
#' @param categories numeric vector of land use categories in observed maps. Only
#'   required if \code{obs} is not provided
#' @param labels character vector (optional) with labels corresponding to
#'   \code{categories}. Only required if \code{obs} is not provided
#' @param \dots additional arguments (none)
#'
#' @return An object of class \code{\link{StatModels}}.
#'
#' @export
#' @rdname StatModels

setGeneric("StatModels", function(models, obs, categories, labels, ...)
           standardGeneric("StatModels"))

#' @rdname StatModels
#' @aliases StatModels,list,ObservedMaps,ANY,ANY-method
setMethod("StatModels", signature(models = "list", obs = "ObservedMaps", categories = "ANY", labels = "ANY"),
          function(models, obs, categories, labels, ...) {
              if (length(models) != length(obs@labels)) stop("")
              new("StatModels", models=models, categories=obs@categories, labels=obs@labels)
          }
)

#' @rdname StatModels
#' @aliases StatModels,list,ANY,ANY,numeric,character-method
setMethod("StatModels", signature(models = "list", obs = "ANY", categories = "numeric", labels = "character"),
          function(models, obs, categories, labels, ...) {
              if (length(models) != length(labels)) stop("")
              if (length(models) != length(categories)) stop("")
              new("StatModels", models=models, categories=categories, labels=labels)
          }
)

## TODO: add slot to StatModels that describes the type of models in slot 'models'
