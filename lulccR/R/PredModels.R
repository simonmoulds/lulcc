#' @include class-PredModels.R predModelType.R
NULL

#' Create a PredModels object
#'
#' Methods to create a \code{\link{PredModels-class}} object.
#'
#' TODO
#' 
#' @param models a list of predictive models
#' @param obs an ObsLulcMaps object
#' @param categories numeric vector of land use categories in observed maps. Only
#'   required if \code{obs} is not provided
#' @param labels character vector (optional) with labels corresponding to
#'   \code{categories}. Only required if \code{obs} is not provided
#' @param \dots additional arguments (none)
#'
#' @return An object of class \code{\link{PredModels}}.
#'
#' @export
#' @rdname PredModels
#'
#' @examples
#'
#' ## Plum Island Ecosystems
#' forest.glm <- glm(forest~1, family=binomial, data=train.data)
#' built.glm <- glm(built~ef_001+ef_002+ef_003, family=binomial, data=train.data)
#' other.glm <- glm(other~ef_001+ef_002, family=binomial, data=train.data)

setGeneric("PredModels", function(models, obs, categories, labels, ...)
           standardGeneric("PredModels"))

#' @rdname PredModels
#' @aliases PredModels,list,ObsLulcMaps,ANY,ANY-method
setMethod("PredModels", signature(models = "list", obs = "ObsLulcMaps", categories = "ANY", labels = "ANY"),
          function(models, obs, categories, labels, ...) {
              if (length(models) != length(obs@labels)) stop("")
              types <- predModelType(x=models)
              new("PredModels", models=models, types=types, categories=obs@categories, labels=obs@labels)
          }
)

#' @rdname PredModels
#' @aliases PredModels,list,ANY,ANY,numeric,character-method
setMethod("PredModels", signature(models = "list", obs = "ANY", categories = "numeric", labels = "character"),
          function(models, obs, categories, labels, ...) {
              if (length(models) != length(labels)) stop("")
              if (length(models) != length(categories)) stop("")
              types <- predModelType(x=models)
              new("PredModels", models=models, types=types, categories=categories, labels=labels)
          }
)

## TODO: add slot to PredModels that describes the type of models in slot 'models'
