#' Class PerformanceMulti
#'
#' An S4 class that extends \code{ROCR::\link[ROCR]{performance-class}} to hold
#' the results of multiple model evaluations.
#'
#' @slot performance list of \code{performance} objects. Each object is
#'   calculated for the corresponding \code{prediction} object held in the
#'   \code{\link{class-PredictionMulti}} object supplied to the constructor
#'   function
#' @slot auc numeric vector containing the area under the ROC curve for each
#'   performance object
#' @slot categories numeric vector of land use categories
#' @slot labels character vector corresponding to \code{categories}
#'
#' @author Simon Moulds
#'
#' @rdname class-PerformanceMulti
#'
#' @export
#' @exportClass PerformanceMulti

setClass("PerformanceMulti",
         slots = c(performance = "list",
                   auc = "numeric",
                   categories = "numeric",
                   labels = "character"),
         validity = function(object) {
             ## TODO
             return(TRUE)
         }
)

