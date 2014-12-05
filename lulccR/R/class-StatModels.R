#' Class StatModels
#'
#' An S4 class to hold multiple mathematical models for different land use
#' categories belonging to the same map.
#'
#' @slot models list of mathematical models
#' @slot categories numeric vector of land use categories
#' @slot labels character vector corresponding to \code{categories}
#'
#' @author Simon Moulds
#'
#' @rdname class-StatModels
#'
#' @export
#' @exportClass StatModels

setClass("StatModels",
         slots = c(models = "list",
                   categories = "numeric",
                   labels = "character"),
         validity = function(object) {
             check1 <- (length(object@models) == length(object@categories))
             if (!check1) stop("")
             check2 <- (length(object@models) == length(object@labels))
             if (!check2) stop("")
             return(TRUE)
         }
)

