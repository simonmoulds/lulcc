#' @include class-ObservedMaps.R
NULL

#' Create an ObservedMaps object
#'
#' Methods to create an ObservedMaps object, which may be created from file, an
#' existing Raster* object or a list of Raster* objects.
#'
#' Observed maps should have the same extent and resolution. Note that the study
#' region is defined by the location of non-NA cells in observed maps.
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
#' @param ... additional arguments to \code{raster::\link[raster]{stack}}
#'
#' @author Simon Moulds
#' @return an ObservedMaps object
#'
#' @export
#' @rdname ObservedMaps
#'
#' @examples
#'
#' ## Plum Islands Ecosystem
#' obs <- ObservedMaps(x=pie,
#'                     pattern="lu",
#'                     categories=c(1,2,3),
#'                     labels=c("forest","built","other"),
#'                     t=c(0,6,14))
#'
#' ## Sibuyan Island
#' obs <- ObservedMaps(x=sibuyan,
#'                     pattern="lu",
#'                     categories=c(1,2,3,4,5),
#'                     labels=c("forest","coconut","grass","rice","other"),
#'                     t=c(0))

#if (!isGeneric("ObservedMaps")) {
setGeneric("ObservedMaps", function(x, pattern, ...)
           standardGeneric("ObservedMaps"))
#}

#' @rdname ObservedMaps
#' @aliases ObservedMaps,missing,character-method
setMethod("ObservedMaps", signature(x = "missing", pattern = "character"),
          function(x, pattern, ...) {
              out <- ObservedMaps(x=".", pattern=pattern, ...)
          }
)

#' @rdname ObservedMaps
#' @aliases ObservedMaps,character,character-method
setMethod("ObservedMaps", signature(x = "character", pattern = "character"), 
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
              out <- ObservedMaps(x=maps, ...)
          }
)

#' @rdname ObservedMaps
#' @aliases ObservedMaps,list,character-method
setMethod("ObservedMaps", signature(x = "list", pattern = "character"),
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
              out <- ObservedMaps(x=maps, ...)
          }
)

#' @rdname ObservedMaps
#' @aliases ObservedMaps,RasterLayer,ANY-method
setMethod("ObservedMaps", signature(x = "RasterLayer", pattern = "ANY"),
          function(x, ...) {
              maps <- raster::stack(x)
              out <- ObservedMaps(x=maps, ...)
          }
)

#' @rdname ObservedMaps
#' @aliases ObservedMaps,RasterStack,ANY-method
setMethod("ObservedMaps", signature(x = "RasterStack", pattern = "ANY"),
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
              out <- new("ObservedMaps", maps=x, t=t, total=total, categories=categories, labels=labels)
          }
)
