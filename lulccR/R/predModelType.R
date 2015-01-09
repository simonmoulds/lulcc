setOldClass("rpart")
setOldClass(c("randomForest.formula","randomForest"))

#' Get type of predictive model
#'
#' This function... model type (as opposed to class)
#'
#' @param x a StatModels object, a model or a list of models
#' @param \dots additional arguments
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
#' @aliases predModelType,StatModels-method
setMethod("predModelType", signature(x = "StatModels"),
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
