#' @include class-ObsLulcRasterStack.R class-ExpVarRasterList.R as.data.frame.R
NULL

#' Extract data to fit predictive models
#'
#' Extract a data.frame containing variables required for fitting predictive
#' models.
#'
#' @param obs an ObsLulcRasterStack object
#' @param ef an ExpVarRasterList object
#' @param cells index of cells to be extracted, which may be a
#'   \code{SpatialPoints*} object or a numeric vector representing cell numbers
#'   (see \code{raster::\link[raster]{extract}})
#' @param ... additional arguments to \link{as.data.frame}
#'
#' @seealso \code{\link[base]{as.data.frame}}, \code{\link{ObsLulcRasterStack}},
#' \code{\link{ExpVarRasterList}}, \code{\link{partition}}
#'
#' @return A data.frame.
#'
#' @export
#' @rdname getPredictiveModelInputData
#' 
#' @examples
#'
#' ## TODO

getPredictiveModelInputData <- function(obs, ef, cells, ...) {
    obsdf <- as.data.frame(obs, cells=cells, ...)
    efdf  <- as.data.frame(ef, cells=cells, ...)
    df    <- cbind(obsdf, efdf)
    df
}
