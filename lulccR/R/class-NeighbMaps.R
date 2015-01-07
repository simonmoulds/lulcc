#' Class NeighbMaps
#'
#' An S4 class for neighbourhood maps. Objects contain all the information
#' used in the original call to the constructor function \code{\link{NeighbMaps}}
#' so they can easily be updated for a new land use map.
#'
#' @slot maps list of RasterLayers showing neighbourhood values for each land use in
#'   \code{categories}
#' @slot weights list of weights matrices
#' @slot fun function used to calculate neighbourhood values
#' @slot focal.args list of all other arguments supplied to
#'   \code{raster::\link[raster]{focal}}
#' @slot categories numeric vector of land use categories for which neighbourhood
#'   maps were calculated
#'
#' @author Simon Moulds
#'
#' @export
#' @exportClass NeighbMaps
#' @rdname NeighbMaps-class

setClass("NeighbMaps",
         representation(
             maps = "list",
             weights = "list",
             fun = "function",
             focal.args = "list",
             categories = "numeric"),
         validity = function(object) {
             return(TRUE)
         }
)

