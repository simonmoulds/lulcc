#' @include class-Predictors.R
NULL

#' Coerce \code{Predictors} object to data.frame
#'
#' This function extracts data from all rasters in a \code{\link{Predictors}}
#' object for a specified timestep (if dynamic or \code{\link{PredictorCall}}
#' objects are present).
#'
#' @param x an object of class \code{\link{Predictors}}
#' @param row.names NULL or a character vector giving the row.names for the
#'   data.frame. Missing values are not allowed
#' @param optional logical. If TRUE, setting row names and converting column
#'   names (to syntactic names: see make.names) is optional
#' @param cells index of cells to be extracted
#' @param timestep numeric indicating the timestep under consideration. Only
#'  relevant if x@@maps contains dynamic predictor variables
#' @param ... additional arguments (none)
#'
#' @seealso \code{\link{Predictors}}, \code{\link{predictorMaps}},
#' \code{\link{PredictorCall}}, \code{\link{partition}}
#' @author Simon Moulds
#' @return data.frame
#'
#' @export
#' @rdname as.data.frame

## setGeneric("as.data.frame", function(x, row.names=NULL, optional=FALSE, cells, timestep=0, ...)
##            standardGeneric("as.data.frame"))

#' @rdname as.data.frame
#' @aliases as.data.frame,Predictors-method
setMethod("as.data.frame", signature(x = "Predictors"),
          function(x, row.names=NULL, optional=FALSE, cells, timestep=0, ...) {
              ix <- timestep + 1
              maps <- c(.getPredictorMaps(x@maps, timestep), lapply(x@calls, function(x) x@map))
              s <- raster::stack(maps, ...) ## this will fail if map characteristics do not agree
              df <- as.data.frame(s[cells], row.names=row.names, optional=optional)
              names(df) <- c(x@map.names, x@call.names)
              df
          }
)

.update.data.frame <- function(x, y, map, cells, timestep, ...) {
    ix <- timestep + 1
    nms <- names(x)
    if (length(y@maps) > 0) {
        dynamic.ix <- which(as.logical(sapply(y@maps, function(x) (nlayers(x) > 1))))
        if (length(dynamic.ix) > 0) {
            s <- raster::stack(lapply(y@maps[dynamic.ix], function(x) x[[ix]]))
            update.vals <- s[cells]
            x[,dynamic.ix] <- update.vals
        }
    }
    if (length(y@calls) > 0) {
        call.ix <- which(names(x) %in% y@call.names)
        calls <- lapply(y@calls, function(x) PredictorCall(x, update.arg=map))
        s <- raster::stack(lapply(calls, function(x) x@map))
        update.vals <- s[cells]
        x[,call.ix] <- update.vals
    }
    names(x) <- nms
    x
}
    
.getPredictorMaps <- function(maps, timestep) {
    index <- timestep + 1
    for (i in 1:length(maps)) {
        s <- maps[[i]]
        n <- raster::nlayers(s)
        if (n == 1) {
            maps[[i]] <- s[[1]]
        } else if (index <= n) {
            maps[[i]] <- s[[index]] 
        } else if (index > n) {
            stop("invalid timestep")
        }
    }
    maps
}

## as.data.frame.Predictors <- function(x, row.names=NULL, optional=FALSE, cells, timestep=0, ...) {
##     ix <- timestep + 1
##     maps <- c(.getPredictorMaps(x@maps, timestep), lapply(x@calls, function(x) x@map))
##     s <- raster::stack(maps, ...) ## this will fail if map characteristics do not agree
##     df <- as.data.frame(s[cells], row.names=row.names, optional=optional)
##     names(df) <- c(x@map.names, x@call.names)
##     df
## }

## ' Update data.frame
## '
## ' Update a data.frame of predictor variables if dynamic variables or variables
## ' from PredictorCall objects are present.
## '
## ' @param x data.frame to be updated
## ' @param y object of class Predictors
## ' @param map RasterLayer to be supplied to \code{\link{PredictorCall}} objects
## ' @param cells index of cells to be extracted. Number of elements must equal the
## '   the number of rows in \code{x}
## ' @param timestep numeric indicating the timestep under consideration
## ' @param ... additional arguments (none)
## '
## ' @name update
## ' @seealso \code{\link{as.data.frame}}
## ' @author Simon Moulds
## ' @return a data.frame
## '
## ' @export

    
