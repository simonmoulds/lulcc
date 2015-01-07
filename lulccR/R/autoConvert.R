#' Automatic land use transitions
#'
#' Perform automatic land use transitions in cells that have been excluded from
#' the main allocation routine. This might happen if cells lie within masked
#' areas and cannot change land use type (in which case \code{autoConvert} sets
#' land use to the land use for the previous timestep), or because they must
#' change according to decision rules (in which case the cell is allocated to the
#' land use with the highest suitability value).
#'
#' @param x numeric vector containing the land use pattern for the current
#'   timestep
#' @param mask numeric vector containing binary values where 0 indicates cells
#'   that are not allowed to change. These are automatically assigned the same
#'   land use category for every timestep
#' @param prob matrix. TODO
#' @param categories numeric vector containing land use categories 
#' @param ... additional arguments (none)
#'
#' @return RasterLayer or numeric vector
#'
#' @useDynLib lulccR
#' @export

autoConvert <- function(x, mask, prob, categories, ...) {
    if (length(x) != length(mask)) stop("mask does not correspond with x")
    vals <- .Call("autoconvert", x, mask, prob, categories)
    ix <- which(!is.na(vals))
    vals <- vals[ix]
    out <- list(ix=ix, vals=vals)
}

## if ( !isGeneric("autoConvert")) {
##     setGeneric("autoConvert", function(lu, mask, prob, categories, ...)
##                    standardGeneric("autoConvert"))
## }

## setMethod("autoConvert", signature(lu = "numeric", mask = "numeric", prob = "matrix", categories = "numeric"),
##           function(lu, mask, prob, categories, ...) {
##               if (length(lu) != length(mask)) stop("mask cells do not correspond with lu cells")
##               vals <- .Call("autoconvert", lu, mask, prob, categories)
##               ix <- which(!is.na(vals))
##               vals <- vals[ix]
##               out <- list(ix=ix, vals=vals)
##               return(out)
##           }
## )

## setMethod("autoconvert", signature(lu = "RasterLayer", mask = "RasterLayer", prob = "matrix", points = "Spatial", categories = "numeric"),
##           function(lu, mask, prob, points, categories, ...) {
##               lu.vals <- extract(lu, points)
##               mask.vals <- extract(mask, points)

##               if (length(lu.vals) != length(mask.vals)) stop("mask cells do not correspond with lu cells")
##               if (length(lu.vals) == 0) stop("Either lu or mask contain no values in the study region")

##               ##cells <- cellFromXY(lu, points)
##               ##out <- lu
##               ##out[cells] <- autoconvert(lu=lu.vals, mask=mask.vals, prob=prob, categories=categories)
##               out <- autoconvert(lu=lu.vals, mask=mask.vals, prob=prob, categories=categories)
##               return(out)
##           }
## )
