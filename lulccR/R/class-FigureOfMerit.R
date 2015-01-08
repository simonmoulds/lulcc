#' Class FigureOfMerit
#'
#' An S4 class for different figure of merit scores.
#'
#' @slot overall list containing the overall figure of merit score for each
#'   aggregation factor
#' @slot category list of numeric vectors containing category specific scores
#' @slot transition list of matrices containing transition specific scores
#' @slot factors numeric vector of aggregation factors
#' @slot categories numeric vector of land use categories
#' @slot labels character vector corresponding to \code{categories}
#' 
#' @export
#' @exportClass FigureOfMerit
#' @rdname FigureOfMerit-class

setClass("FigureOfMerit",
         slots = c(overall = "list",
                   category = "list",
                   transition = "list",
                   factors = "numeric",
                   categories = "numeric",
                   labels = "character"),
         validity = function(object) {
             ## TODO
             return(TRUE)
         }
)
         
