#' Class ObsLulcRasterStack
#'
#' An S4 class for observed land use maps.
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
#' @slot t numeric vector with timesteps corresponding to each observed map
#' @slot categories numeric vector of land use categories
#' @slot labels character vector corresponding to \code{categories}
#' 
#' @export
#' @exportClass ObsLulcRasterStack
#' @rdname ObsLulcRasterStack-class

setClass("ObsLulcRasterStack",
         contains = c("RasterStack",
                      "CategoryLabel"),
         slots = c(t = "numeric"),
                   ## categories = "numeric",
                   ## labels = "character"),         
         validity = function(object) {
             check1 <- (raster::nlayers(object) > 0)
             if (!check1) stop("RasterStack contains no layers")
             check2 <- (length(object@t) == raster::nlayers(object))
             if (!check2) stop("timesteps do not correspond with maps")
             check3 <- all(sort(unique(as.numeric(raster::getValues(object)))) %in% object@categories)
             if (!check3) stop("unknown categories in maps")
             check4 <- (length(object@categories) == length(object@labels))
             if (!check4) stop("labels and categories have different lengths")
             return(TRUE)
         }
)

## setClass("ObsLulcRasterStack",
##          slots = c(maps = "RasterStack",
##                    t = "numeric",
##                    ## total = "matrix",
##                    categories = "numeric",
##                    labels = "character"),         
##          validity = function(object) {
##              check1 <- (raster::nlayers(object@maps) > 0)
##              if (!check1) stop("RasterStack contains no layers")
##              check2 <- (length(object@t) == raster::nlayers(object@maps))
##              if (!check2) stop("timesteps do not correspond with maps")
##              check3 <- all(sort(unique(as.numeric(raster::getValues(object@maps)))) %in% object@categories)
##              if (!check3) stop("unknown categories in maps")
##              check4 <- (length(object@categories) == length(object@labels))
##              if (!check4) stop("labels and categories have different lengths")
##              return(TRUE)
##          }
## )
