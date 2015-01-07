#' @include class-PredictorMaps.R
NULL

#' Resample maps in PredictorMaps object or list 
#'
#' A wrapper function for \code{raster::\link[raster]{resample}} to resample
#' raster objects in a \link{PredictorMaps-class} object or list.
#'
#' @param x a PredictorMaps object or list of Raster* maps to be resampled
#' @param y Raster* object with parameters that \code{x} should be resampled to
#' @param method method used to compute values for the new RasterLayer, should be
#'   \code{"bilinear"} for bilinear interpolation, or \code{"ngb"} for nearest
#'   neighbour
#' @param ... additional arguments to \code{raster::\link[raster]{resample}}
#'
#' @seealso \code{\link{PredictorMaps}}
#' @author Simon Moulds
#' @return PredictorMaps object or list, depending on \code{x}
#'
#' @rdname resample
#'
#' @export
#'
#' @examples
#'
#' ## create PredictorMaps object
#' pred <- predictorMaps(x=pie, pattern="pred")
#' pred <- PredictorMaps(maps=pred)
#'
#' ## resample to ensure maps have same characteristics as observed maps
#' pred <- resample(x=pred, y=pie$lu_pie_1985, method="ngb")

##setGeneric("resample", function(x, y, ...)
##           standardGeneric("resample"))

#' @rdname resample
#' @aliases resample,PredictorMaps,Raster-method
setMethod("resample", signature(x = "PredictorMaps", y = "Raster"),
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
