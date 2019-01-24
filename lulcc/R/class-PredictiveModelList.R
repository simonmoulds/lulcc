#' @include class-CategoryLabel.R
NULL

#' Class PredictiveModelList
#'
#' An S4 class to hold multiple mathematical models for different land use
#' categories belonging to the same map.
#'
#' @slot models list of predictive models
#' @slot categories numeric vector of land use categories
#' @slot labels character vector with labels corresponding to \code{categories}
#'
#' @export
#' @exportClass PredictiveModelList
#' @rdname PredictiveModelList-class
setClass("PredictiveModelList",
         contains = c("CategoryLabel"),
         slots = c(models = "list"),
                   ## prediction = "list",
                   ## performance = "list"),
                   ## types = "character",
                   ## categories = "numeric",
                   ## labels = "character"),
         validity = function(object) {
             check1 <- (length(object@models) == length(object@categories))
             if (!check1) stop("")
             check2 <- (length(object@models) == length(object@labels))
             if (!check2) stop("")
             return(TRUE)
         }
)

