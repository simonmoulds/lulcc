#' @include class-ExpVarMaps.R
NULL

#' Resample maps in ExpVarMaps object or list 
#'
#' A wrapper function for \code{raster::\link[raster]{resample}} to resample
#' raster objects in an ExpVarMaps object or list.
#'
#' @param x an ExpVarMaps object or list of Raster* maps to be resampled
#' @param y Raster* object with parameters that \code{x} should be resampled to
#' @param method method used to compute values for the new RasterLayer, should be
#'   \code{"bilinear"} for bilinear interpolation, or \code{"ngb"} for nearest
#'   neighbour
#' @param \dots additional arguments to \code{raster::\link[raster]{resample}}
#'
#' @seealso \code{\link{ExpVarMaps}}, \code{raster::\link[raster]{resample}}
#'
#' @return An ExpVarMaps object or list, depending on \code{x}.
#'
#' @export
#' @rdname resample
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Plum Island Ecosystems
#'
#' ## observed data
#' obs <- ObsLulcMaps(x=pie,
#'                     pattern="lu",
#'                     categories=c(1,2,3),
#'                     labels=c("forest","built","other"),
#'                     t=c(0,6,14))
#' 
#' ## explanatory variables
#' ef <- ExpVarMaps(x=pie, pattern="ef")
#' 
#' ## resample to ensure maps have same characteristics as observed maps
#' ef <- resample(x=ef, y=obs, method="ngb")
#'
#' }

#' @rdname resample
#' @aliases resample,ExpVarMaps,Raster-method
setMethod("resample", signature(x = "ExpVarMaps", y = "Raster"),
          function(x, y, method="ngb", ...) {
              maps <- x@maps
              ##calls <- x@calls
              resamp.maps <- list()
              if (length(maps) > 0) {
                  for (i in 1:length(maps)) {
                      resamp.maps[[i]] <- raster::resample(x=maps[[i]], y=y, method=method, ...)
                  }
              } else {
                  resamp.maps <- maps
              }

              ## if (length(calls) > 0) {
              ##     for (i in 1:length(calls)) {
              ##         resamp.calls[[i]] <- PredictorCall(call=calls[[i]], update.arg=y)
              ##     }
              ## } else {
              ##     resamp.calls <- calls
              ## }
              
              x@maps <- resamp.maps
              ## x@calls <- resamp.calls
              x
          }
)

#' @rdname resample
#' @aliases resample,list,Raster-method
setMethod("resample", signature(x = "list", y = "Raster"),
          function(x, y, method="ngb", ...) {
              for (i in 1:length(x)) {
                  x[[i]] <- raster::resample(x=x[[i]], y=y, method=method, ...)
              }
              x
          }
)
