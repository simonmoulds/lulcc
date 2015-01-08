#' Class PredictionMulti
#'
#' An S4 class that extends \code{ROCR::\link[ROCR]{prediction-class}} to hold
#' the results of multiple model predictions.
#'
#' @slot prediction a list of \code{ROCR::\link[ROCR]{prediction-class}} objects.
#'   These objects are calculated for each statistical model in the
#'   \code{\link{StatModels}} object supplied to the constructor function
#' @slot categories numeric vector of land use categories for which
#'   \code{prediction} objects were created
#' @slot labels character vector with labels corresponding to \code{categories}
#'
#' @export
#' @exportClass PredictionMulti
#' @rdname PredictionMulti-class

setClass("PredictionMulti",
         representation(
             prediction = "list",
             categories = "numeric",
             labels = "character"),
         validity = function(object) {
             ## TODO
             return(TRUE)
         }
)

