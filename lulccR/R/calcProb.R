setOldClass("rpart")
setOldClass(c("randomForest.formula","randomForest"))

#' Estimate location suitability
#'
#' Estimate location suitability with predictive models.
#'
#' This function is usually called from \code{allocate} to calculate land use
#' suitability at each timestep. However, it may also be used to produce
#' suitability maps (see examples).
#'
#' @param object a \code{PredModels} object or a model of any class
#'   for which a predict method exists (currently, \code{\link[stats]{glm}},
#'   \code{rpart::\link[rpart]{rpart}} and
#'   \code{randomForest::\link[randomForest]{randomForest}}
#' @param newdata data.frame containing new data
#' @param df logical indicating whether the function should return a matrix
#'   (default) or data.frame
#' @param \dots additional arguments to \code{predict}
#'
#' @seealso \code{\link{PredModels}}, \code{\link{allocate}},
#' \code{\link[stats]{glm}}, \code{rpart::\link[rpart]{rpart}},
#' \code{randomForest::\link[randomForest]{randomForest}}
#'
#' @return A matrix or data.frame.
#'
#' @export
#' @rdname calcProb
#'
#' @examples
#' 
#' \dontrun{ 
#'
#' ## Sibuyan Island
#'
#' ## load observed land use data
#' obs <- ObsLulcMaps(x=sibuyan$maps,
#'                     pattern="lu",
#'                     categories=c(1,2,3,4,5),
#'                     labels=c("forest","coconut","grass","rice","other"),
#'                     t=c(0,14))
#'
#' ## load explanatory variables
#' ef <- ExpVarMaps(x=sibuyan$maps, pattern="ef")
#' part <- partition(x=obs@@maps[[1]], size=0.5, spatial=FALSE)
#' efdf <- as.data.frame(x=ef, cells=part$all)
#'
#' ## get glm.models from data
#' glm.models <- sibuyan$intermediate$glm.models
#'
#' probmaps <- calcProb(object=glm.models, newdata=efdf, df=TRUE)
#' points <- rasterToPoints(obs@@maps[[1]], spatial=TRUE)
#' probmaps <- SpatialPointsDataFrame(coords=points, data=probmaps)
#' r <- rasterize(x=probmaps, y=obs@@maps[[1]], field=names(probmaps))
#' plot(stack(r))
#' }

setGeneric("calcProb", function(object, ...)
           standardGeneric("calcProb"))

#' @rdname calcProb
#' @aliases calcProb,glm-method
setMethod("calcProb", signature(object = "glm"),
          function(object, newdata, ...) {
              out <- predict(object=object, newdata=newdata, type="response", ...)
          }
)

#' @rdname calcProb
#' @aliases calcProb,rpart-method
setMethod("calcProb", signature(object = "rpart"),
          function(object, newdata, ...) {
              out <- predict(object=object, newdata=newdata, type="prob", ...)[,2]
          }
)

#' @rdname calcProb
#' @aliases calcProb,randomForest-method
setMethod("calcProb", signature(object = "randomForest"),
          function(object, newdata, ...) {
              out <- predict(object=object, newdata=newdata, type="response", ...) ##[,2]
          }
)

#' @rdname calcProb
#' @aliases calcProb,PredModels-method
setMethod("calcProb", signature(object = "PredModels"),
          function(object, newdata, df=FALSE, ...) {
              out <- list()
              for (i in 1:length(object@models)) {
                  out[[i]] <- calcProb(object=object@models[[i]], newdata=newdata, ...)
              }
              names(out) <- object@labels
              out <- as.data.frame(out)
              if (!df) out <- as.matrix(out)
              out
          }
)
