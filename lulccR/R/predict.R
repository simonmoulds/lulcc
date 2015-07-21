#' Predict location suitability
#'
#' Estimate location suitability with predictive models.
#'
#' This function is usually called from \code{allocate} to calculate land use
#' suitability at each timestep. However, it may also be used to produce
#' suitability maps (see examples).
#'
#' @param object a PredModels object 
#' @param newdata data.frame containing new data
#' @param data.frame logical indicating whether the function should return a
#'   matrix (default) or data.frame
#' @param \dots additional arguments to \code{predict} methods
#'
#' @seealso \code{\link{PredModels}}, \code{\link{allocate}},
#' \code{\link[stats]{glm}}, \code{rpart::\link[rpart]{rpart}},
#' \code{randomForest::\link[randomForest]{randomForest}}
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
#' ## Plum Island Ecosystems
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
#' 
#' ## prepare model input
#' input <- ModelInput(obs=obs,
#'                     ef=ef,
#'                     time=0:14)
#' 
#' ## separate data into training and testing partitions
#' part <- partition(x=obs[[1]], size=0.1, spatial=TRUE)
#' train.data <- as.data.frame(x=input, cells=part[["train"]], t=0)
#' ## test.data  <- as.data.frame(x=input, cells=part[["test"]], t=0,  efonly=TRUE)
#' all.data   <- as.data.frame(x=input, cells=part[["all"]], t=0, efonly=TRUE)
#' 
#' ## get glm.models from data
#' forms <- list(Forest ~ ef_001+ef_002+ef_003+ef_004+ef_005+ef_006+ef_007+ef_008+ef_010+ef_012,
#'               Coconut ~ ef_001+ef_002+ef_005+ef_007+ef_008+ef_009+ef_010+ef_011+ef_012,
#'               Grass~ef_001+ef_002+ef_004+ef_005+ef_007+ef_008+ef_009+ef_010+ef_011+ef_012+ef_013,
#'               Rice~ef_009+ef_010+ef_011,
#'               Other~1)
#' 
#' glm.models <- glmModels(formula=forms, family=binomial, data=train.data, obs=obs)
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
#' @method predict PredModels
#' @export
predict.PredModels <- function(object, newdata, data.frame=FALSE, ...) {
    out <- list()
    for (i in 1:length(object)) {

        mod <- object@models[[i]]
        if (inherits(mod, "randomForest")) {
            out <- predict(object=mod, newdata=newdata, type="response", ...) ##[,2]
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
#' @aliases predict,PredModels-method
setMethod("predict","PredModels",predict.PredModels)

