#' Class NeighbRasterStack
#'
#' An S4 class for neighbourhood maps.
#'
#' @slot filename see \code{raster::\link[raster]{Raster-class}}
#' @slot layers see \code{raster::\link[raster]{Raster-class}}
#' @slot title see \code{raster::\link[raster]{Raster-class}}
#' @slot extent see \code{raster::\link[raster]{Raster-class}}
#' @slot rotated see \code{raster::\link[raster]{Raster-class}}
#' @slot rotation see \code{raster::\link[raster]{Raster-class}}
#' @slot ncols see \code{raster::\link[raster]{Raster-class}}
#' @slot nrows see \code{raster::\link[raster]{Raster-class}}
#' @slot crs see \code{raster::\link[raster]{Raster-class}}
#' @slot history see \code{raster::\link[raster]{Raster-class}}
#' @slot z see \code{raster::\link[raster]{Raster-class}}
#' @slot calls list containing each call to \code{raster::\link[raster]{focal}}
#' @slot categories numeric vector of land use categories for which neighbourhood
#'   maps exist
#'
#' @export
#' @exportClass NeighbRasterStack
#' @rdname NeighbRasterStack-class

setClass("NeighbRasterStack",
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


## setClass("NeighbRasterStack",
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

