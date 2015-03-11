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
#' @param object a PredModels object or a model of any class for which a predict
#'   method exists (currently, \code{\link[stats]{glm}},
#'   \code{rpart::\link[rpart]{rpart}} and
#'   \code{randomForest::\link[randomForest]{randomForest}}
#' @param newdata data.frame containing new data
#' @param df logical indicating whether the function should return a matrix
#'   (default) or data.frame
#' @param \dots additional arguments to \code{predict} methods
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
#'                     labels=c("Forest","Coconut","Grass","Rice","Other"),
#'                     t=c(0,14))
#'
#' ## load explanatory variables
#' ef <- ExpVarMaps(x=sibuyan$maps, pattern="ef")
#' part <- partition(x=obs@@maps[[1]], size=0.5, spatial=FALSE)
#' efdf <- as.data.frame(x=ef, cells=part$all)
#'
#' ## get training data
#' br <- raster::layerize(obs@@maps[[1]]) 
#' names(br) <- obs@@labels
#' train.df <- raster::extract(x=br, y=part$train, df=TRUE)
#' train.df <- cbind(train.df, as.data.frame(x=ef, cells=part$train))
#'
#' ## fit glm models
#' forest.glm <- glm(Forest ~ ef_001+ef_002+ef_003+ef_004+ef_005
#'                            +ef_006+ef_007+ef_008+ef_010+ef_012,
#'                   family=binomial, data=train.df)
#' 
#' coconut.glm <- glm(Coconut ~ ef_001+ef_002+ef_005+ef_007+ef_008
#'                              +ef_009+ef_010+ef_011+ef_012,
#'                    family=binomial, data=train.df)
#' 
#' grass.glm <- glm(Grass ~ ef_001+ef_002+ef_004+ef_005+ef_007
#'                              +ef_008+ef_009+ef_010+ef_011+ef_012+ef_013,
#'                  family=binomial, data=train.df)
#'
#' rice.glm <- glm(Rice~ef_009+ef_010+ef_011,
#'                 family=binomial, data=train.df)
#'
#' other.glm <- glm(Other~1, family=binomial, data=train.df)
#'
#' ## create PredModels object
#' glm.models <- PredModels(models=list(forest.glm, coconut.glm, grass.glm,
#'                                      rice.glm, other.glm),
#'                          obs=obs)
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
