## if (!isGeneric("predictorMaps")) {
##     setGeneric("predictorMaps", function(x, pattern, ...)
##                standardGeneric("predictorMaps"))
## }

#' Load predictor variables
#'
#' Methods to load maps of predictor variables, which may be created from file, an
#' existing Raster* object or a list of Raster* objects.
#'
#' Predictor maps should follow a naming convention to identify them as static
#' (one map provided for the study period) or dynamic (one map provided for each
#' year of the study period). The name should consist of two (static) or three
#' (dynamic) parts: firstly, the prefix should differentiate predictor maps from
#' other maps in the directory, list or RasterStack. This should be followed
#' by a unique number to differentiate the predictor maps (note that the order of
#' predictor variables in the predictorMaps object is determined by this value)
#' If the predictor is dynamic this number should be followed by a second number
#' representing the timestep to which the map applies. Dynamic variables should
#' include a map for time 0 (corresponding to the initial observed map) and every
#' subsequent timestep. The different parts should be separated by a period or
#' underscore.  
#'
#' Maps of different predictor variables should have the same coordinate
#' reference system but do not have to have the same extent and resolution as 
#' long as the minimum extent is that of the study region defined by an
#' \code{ObservedMaps} object. However, maps for different timesteps of the same
#' dynamic predictor variable should have the same extent and resolution because
#' these are stored as RasterStack objects.
#' 
#' @param x path (character) to directory containing observed land use maps,
#'   a Raster* object or a list of Raster* objects. 
#' @param pattern regular expression (character). Only filenames (if \code{x} is
#'   a path) or Raster* objects (if \code{x} is a list) matching the regular
#'   expression will be returned. See \code{raster::\link{raster}} for more
#'   information about supported filetypes
#' @param ... additional arguments to \code{raster::\link[raster]{stack}}
#'
#' @seealso \code{\link{Predictors}}
#' @author Simon Moulds
#' @return a predictorMaps object
#'
#' @export
#' @rdname predictorMaps
#'
#' @examples
#'
#' ## Plum Island Ecosystem
#' pred.maps <- PredictorMaps(x=pie, pattern="pred")
#'
#' ## Sibuyan
#' pred.maps <- PredictorMaps(x=sibuyan, pattern="pred")

#if (!isGeneric("predictorMaps")) {
setGeneric("predictorMaps", function(x, pattern, ...)
           standardGeneric("predictorMaps"))
#}

#' @rdname predictorMaps
#' @aliases predictorMaps,missing,character-method
setMethod("predictorMaps", signature(x = "missing", pattern = "character"),
          function(x, pattern, ...) {
              out <- predictorMaps(x=".", pattern=pattern, ...)
          }
)

#' @rdname predictorMaps
#' @aliases predictorMaps,character,character-method
setMethod("predictorMaps", signature(x = "character", pattern = "character"),
          function(x, pattern, ...) {
              files <- list.files(path=x, pattern=pattern, full.names=FALSE)
              if (length(files) > 0) {
                  ##files <- mixedsort(files)
                  files <- sort(files)
                  paths <- file.path(x, files)
                  maps <- raster::stack(paths, ...);
                  nms <- names(maps)
                  maps <- raster::unstack(maps)
                  names(maps) <- nms
                  out <- .getPredMaps(maps)                  
              } else {
                  stop("no predictor maps found")
              }
              out
          }
)

#' @rdname predictorMaps
#' @aliases predictorMaps,list,character-method
setMethod("predictorMaps", signature(x = "list", pattern = "character"),
          function(x, pattern, ...) {
              list.names <- names(x)
              if (is.null(list.names)) stop("list elements must be named")
              ix <- grep(pattern=pattern, x=list.names)
              files <- list.names[ix]
              files <- sort(files)
              maps <- x[files]              
              out <- .getPredMaps(maps)
          }
)

#' @rdname predictorMaps
#' @aliases predictorMaps,RasterStack,character-method
setMethod("predictorMaps", signature(x = "RasterStack", pattern = "character"),
          function(x, pattern, ...) {
              stack.names <- names(x)
              x <- raster::unstack(x)
              names(x) <- stack.names
              out <- predictorMaps(x=x, pattern=pattern)
          }
)

.getPredMaps <- function(maps) {
    nms <- names(maps)               
    ids <- gsubfn::strapply(nms, "[0-9]*\\d")
    ids <- sapply(ids, function(x) x[1])
    unique.ids <- unique(ids) 
    maps2 <- list()
    for (i in 1:length(unique.ids)) {
        id <- unique.ids[i]
        maps2[[i]] <- stack(maps[ids %in% id])
    }
    prefix <- strsplit(nms, ids)
    prefix <- sapply(prefix, function(x) x[1])
    names(maps2) <- unique(paste0(prefix, ids))
    maps2
}
