#' @include class-PredModels.R
NULL

#' Create a PredModels object
#'
#' Methods to create a \code{\link{PredModels-class}} object.
#'
#' @param models a list of predictive models corresponding to
#'   \code{obs@@categories} or \code{categories}
#' @param obs an ObsLulcMaps object
#' @param categories numeric vector of land use categories in observed maps. Only
#'   required if \code{obs} is not provided
#' @param labels character vector (optional) with labels corresponding to
#'   \code{categories}. Only required if \code{obs} is not provided
#' @param \dots additional arguments (none)
#'
#' @seealso \code{\link[stats]{glm}}, \code{rpart::\link[rpart]{rpart}},
#' \code{randomForest::\link[randomForest]{randomForest}}
#'
#' @return A \code{\link{PredModels}} object.
#'
#' @export
#' @rdname PredModels
#'
#' @examples
#'
#' ## see lulccR-package examples

setGeneric("PredModels", function(models, obs, categories, labels, ...)
           standardGeneric("PredModels"))

#' @rdname PredModels
#' @aliases PredModels,list,ObsLulcMaps,ANY,ANY-method
setMethod("PredModels", signature(models = "list", obs = "ObsLulcMaps", categories = "ANY", labels = "ANY"),
          function(models, obs, categories, labels, ...) {
              if (length(models) != length(obs@labels)) stop("'models' must contain a model for each category in 'obs'")

              ## types <- predModelType(x=models)
              ## new("PredModels", models=models, types=types, categories=obs@categories, labels=obs@labels)

              new("PredModels", models=models, categories=obs@categories, labels=obs@labels)
          }
)

#' @rdname PredModels
#' @aliases PredModels,list,ANY,ANY,numeric,character-method
setMethod("PredModels", signature(models = "list", obs = "ANY", categories = "numeric", labels = "character"),
          function(models, obs, categories, labels, ...) {
              if (length(models) != length(labels)) stop("")
              if (length(models) != length(categories)) stop("")

              ## types <- predModelType(x=models)
              ## new("PredModels", models=models, types=types, categories=categories, labels=labels)
              
              new("PredModels", models=models, categories=categories, labels=labels)
          }
)
