#' @include class-CategoryLabel.R
NULL

#' Class PerformanceList
#'
#' An S4 class that extends \code{ROCR::\link[ROCR]{performance-class}} to hold
#' the results of multiple model evaluations.
#'
#' @slot performance list of ROCR performance objects. Each object is
#'   calculated for the corresponding ROCR prediction object held in the
#'   PredictionList object supplied to the constructor function
#' @slot auc numeric vector containing the area under the curve for each
#'   performance object
#' @slot categories numeric vector of land use categories for which performance
#'   objects were created
#' @slot labels character vector with labels corresponding to \code{categories}
#'
#' @export
#' @exportClass PerformanceList
#' @rdname PerformanceList-class

setClass("PerformanceList",
         contains = c("CategoryLabel"),
         slots = c(performance = "list",
                   auc = "numeric"),
                   ## types = "character",
                   ## categories = "numeric",
                   ## labels = "character"),
         validity = function(object) {
             ## TODO
             return(TRUE)
         }
)

