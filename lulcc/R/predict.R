#' Predict location suitability
#'
#' Estimate location suitability with predictive models.
#'
#' This function is usually called from \code{allocate} to calculate land use
#' suitability at each timestep. However, it may also be used to produce
#' suitability maps (see examples).
#'
#' @param object a PredictiveModelList object 
#' @param newdata data.frame containing new data
#' @param data.frame logical indicating whether the function should return a
#'   matrix (default) or data.frame
#' @param \dots additional arguments to \code{predict} methods
#'
#' @seealso \code{\link{Model fitting}}, \code{\link{allocate}}
#'
#' @return A matrix.
#'
#' @export
#' @rdname predict
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Sibuyan Island
#' 
#' ## load observed land use data
#' obs <- ObsLulcRasterStack(x=sibuyan$maps,
#'                     pattern="lu",
#'                     categories=c(1,2,3,4,5),
#'                     labels=c("Forest","Coconut","Grass","Rice","Other"),
#'                     t=c(0,14))
#' 
#' ## load explanatory variables
#' ef <- ExpVarRasterList(x=sibuyan$maps, pattern="ef")
#' 
#' ## separate data into training and testing partitions
#' part <- partition(x=obs[[1]], size=0.1, spatial=TRUE)
#' train.data <- getPredictiveModelInputData(obs=obs, ef=ef, cells=part[["train"]])
#' all.data <- getPredictiveModelInputData(obs=obs, ef=ef, cells=part[["all"]])
#' 
#' ## get glm.models from data
#' forms <- list(Forest ~ ef_001+ef_002+ef_003+ef_004+ef_005+ef_006+ef_007+ef_008+ef_010+ef_012,
#'               Coconut ~ ef_001+ef_002+ef_005+ef_007+ef_008+ef_009+ef_010+ef_011+ef_012,
#'               Grass~ef_001+ef_002+ef_004+ef_005+ef_007+ef_008+ef_009+ef_010+ef_011+ef_012+ef_013,
#'               Rice~ef_009+ef_010+ef_011,
#'               Other~1)
#' 
#' glm.models <- glmModels(formula=forms, family=binomial, data=train.data, obs=obs)
#' 
#' ## create suitability maps
#' suitability.maps <- predict(object=glm.models, newdata=all.data, data.frame=TRUE)
#' points <- rasterToPoints(obs[[1]], spatial=TRUE)
#' suitability.maps <- SpatialPointsDataFrame(coords=points, data=suitability.maps)
#' r <- stack(rasterize(x=suitability.maps, y=obs[[1]], field=names(suitability.maps)))
#' plot(r)
#' 
#' ## library(rasterVis)
#' ## levelplot(r)
#' 
#' }

#' @rdname predict
#' @method predict PredictiveModelList
#' @export
predict.PredictiveModelList <- function(object, newdata, data.frame=FALSE, ...) {
    out <- list()
    for (i in 1:length(object)) {

        mod <- object@models[[i]]
        if (inherits(mod, "randomForest")) {
            out[[i]] <- predict(object=mod, newdata=newdata, type="response", ...) ##[,2]
        }

        if (inherits(mod, "rpart")) {
            out[[i]] <- predict(object=mod, newdata=newdata, type="prob", ...)[,2]
        }

        if (inherits(mod, "glm")) {
            out[[i]] <- predict(object=mod, newdata=newdata, type="response")#, ...)
        }
        ## out[[i]] <- predict(object=object@models[[i]], newdata=newdata, ...)
    }

    names(out) <- object@labels
    out <- as.data.frame(out)
    if (!data.frame) out <- as.matrix(out)
    out
}

#' @rdname predict
#' @aliases predict,PredictiveModelList-method
setMethod("predict","PredictiveModelList",predict.PredictiveModelList)

