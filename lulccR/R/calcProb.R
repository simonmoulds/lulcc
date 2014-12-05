setOldClass("rpart")
setOldClass(c("randomForest.formula","randomForest"))

if (!isGeneric("calcProb")) {
    setGeneric("calcProb", function(object, ...)
               standardGeneric("calcProb"))
}

#' Estimate location suitability
#'
#' Estimate location suitability with predictive models.
#' 
#' @param object a \code{StatModels} object or a model of any class
#'   for which a predict method exists (currently, \code{glm},
#'   \code{rpart::\link[rpart]{rpart}} and
#'   \code{randomForest::\link[randomForest]{randomForest}}
#' @param newdata data.frame containing new data
#' @param ... additional arguments to \code{predict}
#'
#' @seealso \code{\link{StatModels}}, \code{\link{predict}}
#' @author Simon Moulds
#' @return a data.frame
#'
#' @rdname calcProb
#'
#' @export

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
#' @aliases calcProb,StatModels-method
setMethod("calcProb", signature(object = "StatModels"),
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
