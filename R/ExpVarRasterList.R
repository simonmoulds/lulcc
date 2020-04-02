#' @include class-ExpVarRasterList.R
NULL

#' Create an ExpVarRasterList object
#'
#' Methods to load maps of explanatory variables, which may be created from file,
#' an existing Raster* object or a list of Raster* objects.
#'
#' Explanatory variables should follow a naming convention to identify them as
#' static (one map provided for the study period) or dynamic (one map provided
#' for each year of the study period). The name should consist of two (static)
#' or three (dynamic) parts: firstly, the prefix should differentiate explanatory
#' variables from other maps in the directory, list or RasterStack. This should
#' be followed by a unique number to differentiate the explanatory variables
#' (note that the order of variables in the ExpVarRasterList object is determined by
#' this value) If the variable is dynamic this number should be followed by a
#' second number representing the timestep to which the map applies. Dynamic
#' variables should include a map for time 0 (corresponding to the initial
#' observed map) and every subsequent timestep in the simulation. The different
#' parts should be separated by a period or underscore.  
#'
#' Maps of different explanatory variables should have the same coordinate
#' reference system but do not have to have the same extent and resolution as
#' long as the minimum extent is that of the study region defined by an
#' \code{ObsLulcRasterStack} object. However, maps for different timesteps of the same
#' dynamic variable should have the same extent and resolution because these are
#' stored as RasterStack objects.
#' 
#' @param x path (character) to directory containing observed land use maps,
#'   a Raster* object or a list of Raster* objects
#' @param pattern regular expression (character). Only filenames (if \code{x} is
#'   a path) or Raster* objects (if \code{x} is a list) matching the regular
#'   expression will be returned. See \cr
#'   \code{raster::\link[raster]{raster}} for more information about supported
#'   filetypes
#' @param \dots additional arguments to \code{raster::\link[raster]{stack}}
#'
#' @seealso \code{raster::\link[raster]{stack}}
#' @return An ExpVarRasterList object.
#'
#' @export
#' @rdname ExpVarRasterList
#'
#' @examples
#'
#' ## Plum Island Ecosystems
#' ef <- ExpVarRasterList(x=pie, pattern="ef")
#' 
#' ## Sibuyan
#' ef <- ExpVarRasterList(x=sibuyan$maps, pattern="ef")
#'

setGeneric("ExpVarRasterList", function(x, ...)
           standardGeneric("ExpVarRasterList"))

#' @rdname ExpVarRasterList
#' @aliases ExpVarRasterList,missing,character-method
setMethod("ExpVarRasterList", signature(x = "missing"),
          function(x, pattern = NULL, ...) {
              out <- ExpVarRasterList(x=".", pattern=pattern, ...)
          }
)

#' @rdname ExpVarRasterList
#' @aliases ExpVarRasterList,character,character-method
setMethod("ExpVarRasterList", signature(x = "character"),
          function(x, pattern = NULL, ...) {
              files <- list.files(path=x, pattern=pattern, full.names=FALSE)
              if (length(files) > 0) {
                  ##files <- mixedsort(files)
                  files <- sort(files)
                  paths <- file.path(x, files)
                  x <- raster::stack(paths, ...);
                  nms <- names(x)
                  x <- raster::unstack(x)
                  names(x) <- nms
                  out <- ExpVarRasterList(x=x, ...)
                  #out <- .getPredMaps(maps)                  
              } else {
                  stop("no predictor maps found")
              }
              out
          }
)

#' @rdname ExpVarRasterList
#' @aliases ExpVarRasterList,RasterStack,character-method
setMethod("ExpVarRasterList", signature(x = "RasterStack"),
          function(x, pattern = NULL, ...) {
              stack.names <- names(x)
              x <- raster::unstack(x)
              names(x) <- stack.names
              out <- ExpVarRasterList(x=x, pattern=pattern)
          }
)

#' @rdname ExpVarRasterList
#' @aliases ExpVarRasterList,list,character-method
setMethod("ExpVarRasterList", signature(x = "list"),
          function(x, pattern = NULL, ...) {
              list.names <- names(x)
              if (is.null(list.names)) stop("list elements must be named")

              if (!is.null(pattern)) {
                  ix <- grep(pattern=pattern, x=list.names)
              } else {
                  ix <- seq(1:length(x))
              }
              
              ##ix <- grep(pattern=pattern, x=list.names)
              files <- list.names[ix]
              files <- sort(files)
              maps <- x[files]              
              maps <- .getPredMaps(maps)
              dynamic <- FALSE
              if (max(sapply(maps, nlayers)) > 1) dynamic <- TRUE
              out <- new("ExpVarRasterList",
                         maps=maps,
                         names=as.character(names(maps)),
                         dynamic=dynamic)
              
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
