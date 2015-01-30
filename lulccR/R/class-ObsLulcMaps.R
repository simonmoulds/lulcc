#' Class ObsLulcMaps
#'
#' An S4 class for observed land use maps.
#'
#' @slot maps RasterStack containing observed land use maps
#' @slot t numeric vector with timesteps corresponding to each observed map
#' @slot total matrix with number of cells belonging to each land use category in
#'   the observed maps. Columns represent land use categories and rows represent
#'   different maps
#' @slot categories numeric vector of land use categories
#' @slot labels character vector corresponding to \code{categories}
#' 
#' @export
#' @exportClass ObsLulcMaps
#' @rdname ObsLulcMaps-class

setClass("ObsLulcMaps",
         slots = c(maps = "RasterStack",
                   t = "numeric",
                   total = "matrix",
                   categories = "numeric",
                   labels = "character"),         
         validity = function(object) {
             check1 <- (raster::nlayers(object@maps) > 0)
             if (!check1) stop("RasterStack contains no layers")
             check2 <- (length(object@t) == raster::nlayers(object@maps))
             if (!check2) stop("timesteps do not correspond with maps")
             check3 <- all(sort(unique(as.numeric(raster::getValues(object@maps)))) %in% object@categories)
             if (!check3) stop("unknown categories in maps")
             check4 <- (length(object@categories) == length(object@labels))
             if (!check4) stop("labels and categories have different lengths")
             return(TRUE)
         }
)
