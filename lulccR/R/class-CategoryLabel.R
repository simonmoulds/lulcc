#' Virtual class Model
#'
#' A virtual S4 class to represent land use change models.
#'
#' @slot categories numeric vector of land use categories 
#' @slot labels character vector corresponding to \code{categories}
#'
#' @export
#' @exportClass CategoryLabel
#' @rdname CategoryLabel-class

setClass("CategoryLabel",
         contains = c("VIRTUAL"),
         slots = c(categories = "numeric",
                   labels = "character"),
         validity = function(object) {
             ## TODO
             return(TRUE)
         }
)
