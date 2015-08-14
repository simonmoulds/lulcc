#' @include class-CategoryLabel.R
NULL

#' Class PredictionList
#'
#' An S4 class that extends \code{ROCR::\link[ROCR]{prediction-class}} to hold
#' the results of multiple model predictions.
#'
#' @slot prediction a list of \code{ROCR::\link[ROCR]{prediction-class}} objects.
#'   These objects are calculated for each statistical model in the
#'   \code{PredictiveModelList} object supplied to the constructor function
#' @slot categories numeric vector of land use categories for which
#'   \code{prediction} objects were created
#' @slot labels character vector with labels corresponding to \code{categories}
#'
#' @export
#' @exportClass PredictionList
#' @rdname PredictionList-class

setClass("PredictionList",
         contains = c("CategoryLabel"),
         slots = c(prediction = "list"),
         validity = function(object) {
             ## TODO
             return(TRUE)
         }
)

