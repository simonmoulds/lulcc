#' @include calcProb.R
NULL

#' Get predictive model type
#'
#' This function returns the type of predictive model in use. 
#'
#' @param x a PredModels object, a model or a list of models
#' @param \dots additional arguments (none)
#'
#' @return Character.
#'
#' @export
#' @rdname predModelType
setGeneric("predModelType", function(x, ...)
           standardGeneric("predModelType"))

#' @rdname predModelType
#' @aliases predModelType,glm-method
setMethod("predModelType", signature(x = "glm"), function(x, ...) "glm")

#' @rdname predModelType
#' @aliases predModelType,rpart-method
setMethod("predModelType", signature(x = "rpart"), function(x, ...) "rpart")

#' @rdname predModelType
#' @aliases predModelType,randomForest-method
setMethod("predModelType", signature(x = "randomForest"), function(x, ...) "randomForest")

#' @rdname predModelType
#' @aliases predModelType,PredModels-method
setMethod("predModelType", signature(x = "PredModels"),
          function(x, ...) {
              types <- predModelType(x@models)
              types
          }
)

#' @rdname predModelType
#' @aliases predModelType,list-method
setMethod("predModelType", signature(x = "list"),
          function(x, ...) {
              types <- rep(NA, length(x))
              for (i in 1:length(x)) {
                  types[i] <- predModelType(x[[i]])
              }
              types
          }
)
