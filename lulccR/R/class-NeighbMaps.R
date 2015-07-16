#' Class NeighbMaps
#'
#' An S4 class for neighbourhood maps.
#'
#' @slot filename TODO
#' @slot layers TODO
#' @slot title TODO
#' @slot extent TODO
#' @slot rotated TODO
#' @slot rotation TODO
#' @slot ncols TODO
#' @slot nrows TODO
#' @slot crs TODO
#' @slot history TODO
#' @slot z TODO
#' @slot calls list
#' @slot categories numeric vector of land use categories for which neighbourhood
#'   maps exist
#'
#' @export
#' @exportClass NeighbMaps
#' @rdname NeighbMaps-class

setClass("NeighbMaps",
         contains = c("RasterStack"),
         slots = c(calls = "list",
                   categories = "numeric"),
                   ## weights = "list",
                   ## fun = "function",
                   ## focal.args = "list"),
         validity = function(object) {
             ## TODO
             return(TRUE)
         }
)


## setClass("NeighbMaps",
##          representation(
##              maps = "list",
##              categories = "numeric",
##              weights = "list",
##              fun = "function",
##              focal.args = "list"),
##          validity = function(object) {
##              ## TODO
##              return(TRUE)
##          }
## )

