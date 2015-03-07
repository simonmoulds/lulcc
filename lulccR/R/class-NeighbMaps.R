#' Class NeighbMaps
#'
#' An S4 class for neighbourhood maps.
#'
#' @slot maps list of RasterLayers showing neighbourhood values for each land use
#' in \code{categories}
#' @slot categories numeric vector of land use categories for which neighbourhood
#'   maps exist
#' @slot weights list of weights matrices
#' @slot fun function used to calculate neighbourhood values
#' @slot focal.args list of all other arguments supplied to
#'   \code{raster::\link[raster]{focal}}
#'
#' @export
#' @exportClass NeighbMaps
#' @rdname NeighbMaps-class

setClass("NeighbMaps",
         representation(
             maps = "list",
             categories = "numeric",
             weights = "list",
             fun = "function",
             focal.args = "list"),
         validity = function(object) {
             ## TODO
             return(TRUE)
         }
)

