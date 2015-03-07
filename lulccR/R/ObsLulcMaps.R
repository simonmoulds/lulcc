#' @include class-ObsLulcMaps.R
NULL

#' Create an ObsLulcMaps object
#'
#' Methods to create an ObsLulcMaps object, which may be created from file, an
#' existing Raster* object or a list of Raster* objects.
#'
#' Observed land use maps should have the same extent and resolution. The
#' location of non-NA cells in \code{ObsLulcMaps} objects defines the region for
#' subsequent analysis.
#' 
#' @param x path (character), Raster* object or list of Raster* objects. Default
#'   behaviour is to search for files in the working directory
#' @param pattern regular expression (character). Only filenames (if \code{x} is
#'   a path) or Raster* objects (if \code{x} is a list) matching the regular
#'   expression will be returned. See \code{raster::\link[raster]{raster}} for
#'   more information about supported filetypes
#' @param categories numeric vector of land use categories in observed maps
#' @param labels character vector (optional) with labels corresponding to
#'   \code{categories}
#' @param t numeric vector containing the timestep of each observed map. The 
#'   first timestep must be 0
#' @param \dots additional arguments to \code{raster::\link[raster]{stack}}
#'
#' @return An ObsLulcMaps object.
#'
#' @export
#' @rdname ObsLulcMaps
#'
#' @examples
#'
#' ## Plum Island Ecosystems
#' obs <- ObsLulcMaps(x=pie,
#'                    pattern="lu",
#'                    categories=c(1,2,3),
#'                    labels=c("forest","built","other"),
#'                    t=c(0,6,14))
#'
#' ## Sibuyan Island
#' obs <- ObsLulcMaps(x=sibuyan,
#'                    pattern="lu",
#'                    categories=c(1,2,3,4,5),
#'                    labels=c("forest","coconut","grass","rice","other"),
#'                    t=c(0,14))

setGeneric("ObsLulcMaps", function(x, pattern, ...)
           standardGeneric("ObsLulcMaps"))

#' @rdname ObsLulcMaps
#' @aliases ObsLulcMaps,missing,character-method
setMethod("ObsLulcMaps", signature(x = "missing", pattern = "character"),
          function(x, pattern, ...) {
              out <- ObsLulcMaps(x=".", pattern=pattern, ...)
          }
)

#' @rdname ObsLulcMaps
#' @aliases ObsLulcMaps,character,character-method
setMethod("ObsLulcMaps", signature(x = "character", pattern = "character"), 
          function(x, pattern, ...) {
              files <- list.files(path=x, pattern=pattern, full.names=FALSE)
              if (length(files) > 0) {
                  ##files <- mixedsort(obs.files)
                  files <- sort(files)
                  paths <- file.path(x, files)
                  maps <- raster::stack(paths)                  
              } else {
                  stop("maps not found")
              }
              out <- ObsLulcMaps(x=maps, ...)
          }
)

#' @rdname ObsLulcMaps
#' @aliases ObsLulcMaps,list,character-method
setMethod("ObsLulcMaps", signature(x = "list", pattern = "character"),
           function(x, pattern, ...) {
              list.names <- names(x)
              if (is.null(list.names)) stop("list elements must be named")
              ix <- grep(pattern=pattern, x=list.names)
              files <- list.names[ix]
              ##obs.files <- mixedsort(obs.files)
              files <- sort(files)
              if (length(files) > 0) {
                  maps <- raster::stack(x[files])
              } else {
                  stop("maps not found")
              }
              out <- ObsLulcMaps(x=maps, ...)
          }
)

#' @rdname ObsLulcMaps
#' @aliases ObsLulcMaps,RasterLayer,ANY-method
setMethod("ObsLulcMaps", signature(x = "RasterLayer", pattern = "ANY"),
          function(x, ...) {
              maps <- raster::stack(x)
              out <- ObsLulcMaps(x=maps, ...)
          }
)

#' @rdname ObsLulcMaps
#' @aliases ObsLulcMaps,RasterStack,ANY-method
setMethod("ObsLulcMaps", signature(x = "RasterStack", pattern = "ANY"),
          function(x, pattern, categories, labels, t) {
              if (missing(categories)) categories <- sort(unique(as.numeric(raster::getValues(x))))
              ix <- order(categories)
              categories <- categories[ix]
              if (missing(labels)) {
                  labels <- paste0("cat", categories)
              } else {
                  labels <- labels[ix]
              }
              if (missing(t)) t <- 0
              info <- total(x, categories)
              total <- info$total
              out <- new("ObsLulcMaps", maps=x, t=t, total=total, categories=categories, labels=labels)
          }
)
