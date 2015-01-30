#' Class Performance
#'
#' An S4 class that extends \code{ROCR::\link[ROCR]{performance-class}} to hold
#' the results of multiple model evaluations.
#'
#' @slot performance list of \code{performance} objects. Each object is
#'   calculated for the corresponding \code{prediction} object held in the
#'   \code{\linkS4class{Prediction}} object supplied to the constructor
#'   function
#' @slot auc numeric vector containing the area under the ROC curve for each
#'   performance object
#' @slot types character vector of model types for which \code{performance}
#'   objects were created
#' @slot categories numeric vector of land use categories for which
#'   \code{performance} objects were created
#' @slot labels character vector with labels corresponding to \code{categories}
#'
#' @export
#' @exportClass Performance
#' @rdname Performance-class

setClass("Performance",
         slots = c(performance = "list",
                   auc = "numeric",
                   types = "character",
                   categories = "numeric",
                   labels = "character"),
         validity = function(object) {
             ## TODO
             return(TRUE)
         }
)

