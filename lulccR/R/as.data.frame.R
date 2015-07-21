#' @include class-ExpVarMaps.R class-ModelInput.R
NULL

#' Coerce objects to data.frame
#'
#' This function extracts data from all raster objects in a
#' \code{\link{ExpVarMaps}} object for a specified timestep (if dynamic
#' variables are present).
#'
#' @param x an ExpVarMaps or ModelInput object
#' @param row.names NULL or a character vector giving the row.names for the
#'   data.frame. Missing values are not allowed
#' @param optional logical. If TRUE, setting row names and converting column
#'   names (to syntactic names: see make.names) is optional
#' @param cells index of cells to be extracted, which may be a
#'   \code{SpatialPoints*} object or a numeric vector representing cell numbers
#'   (see \code{raster::\link[raster]{extract}})
#' @param t numeric indicating the time under consideration. Only
#'   relevant if 'x' contains dynamic explanatory variables
#' @param efonly Should the data.frame contain the response variable (i.e.
#'   observed land use) as well as explanatory variables, or just explanatory
#'   variables? Only relevant if 'x' is a \code{ModelInput} object
#' @param \dots additional arguments (none)
#'
#' @seealso \code{\link[base]{as.data.frame}}, \code{\link{ExpVarMaps}},
#' \code{link{ModelInput}}, \code{\link{partition}}
#'
#' @return A data.frame.
#'
#' @export
#' @rdname as.data.frame
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Plum Island Ecosystems
#' 
#' ## observed maps
#' obs <- ObsLulcMaps(x=pie,
#'                    pattern="lu", 
#'                    categories=c(1,2,3), 
#'                    labels=c("Forest","Built","Other"), 
#'                    t=c(0,6,14))
#' 
#' ## explanatory variables
#' ef <- ExpVarMaps(x=pie, pattern="ef")
#' 
#' ## prepare model input
#' input <- ModelInput(obs=obs,
#'                     ef=ef,
#'                     time=0:14)
#' 
#' ## separate data into training and testing partitions
#' part <- partition(x=obs[[1]], size=0.1, spatial=TRUE)
#' train.data <- as.data.frame(x=input, cells=part[["train"]], t=0)
#' test.data  <- as.data.frame(x=input, cells=part[["test"]], t=0,  efonly=TRUE)
#' 
#' names(train.data)
#' names(test.data)
#'
#' }

#' @rdname as.data.frame
#' @method as.data.frame ModelInput
#' @export
as.data.frame.ModelInput <- function(x, row.names=NULL, optional=FALSE, cells, t=0,efonly=FALSE, ...) {

    ## use sp object to extract data because this allows different extents
    if (!inherits(cells, "SpatialPoints")) {
        cells <- xyFromCell(x@obs, cell=cells, spatial=TRUE)
    }

    ## check raster characteristics of explanatory variables
    cr <- sapply(x@ef@maps, FUN=function(y) compareRaster(x@obs, y, extent=FALSE, stopiffalse=FALSE, showwarning=TRUE))             

    if (all(cr)) {
        efdf <- as.data.frame(x=x@ef, cells=cells, t=t)  ## extract using 'simple' method
        
    } else {
        maps <- .getExpVarMaps(x@ef@maps, t=t)
        efdf <- as.data.frame(matrix(data=NA, nrow=length(cells), ncol=length(x@ef)))
        names(efdf) <- names(x@ef)

        for (i in 1:length(x@ef)) {
            if (cr[i]) {
                efdf[,i] <- extract(x=maps[[i]], y=cells)
            } else {
                ## warning(paste0("explanatory variable ", names(x@ef)[i], ""))
                efdf[,i] <- extract(x=maps[[i]], y=cells, method="bilinear", ...)
            }
        }               
    }

    ## give user the option of only extracting explanatory variables
    if (!efonly) {
        if (!t %in% x@obs@t) {
            warning("no observed map at time 't': using setting t = 0 instead")
            t <- 0
        }

        ix <- which(x@obs@t %in% t)
    
        br <- raster::layerize(x@obs[[ix]])
        names(br) <- x@obs@labels
        obsdf <- raster::extract(x=br, y=cells)
        df <- cbind(obsdf, efdf)
    } else {
        df <- efdf
    }

    df
}

#' @rdname as.data.frame
#' @method as.data.frame ExpVarMaps
#' @export
as.data.frame.ExpVarMaps <- function(x, row.names=NULL, optional=FALSE, cells, t=0, ...) {
    ix <- t + 1
    ##maps <- c(.getExpVarMaps(x@maps, t), lapply(x@calls, function(x) x@map))
    maps <- .getExpVarMaps(x@maps, t)
    df <- as.data.frame(matrix(data=NA, nrow=length(cells), ncol=length(maps)))
    for (i in 1:length(maps)) {
        df[,i] <- extract(maps[[i]], cells, ...)
    }
    
    ## s <- raster::stack(maps, ...) ## this will fail if map characteristics do not agree
    ## df <- as.data.frame(s[cells], row.names=row.names, optional=optional)
    names(df) <- x@varnames
    df
}

#' @rdname as.data.frame
#' @aliases as.data.frame,ExpVarMaps-method
setMethod("as.data.frame","ExpVarMaps",as.data.frame.ExpVarMaps)

#' @rdname as.data.frame
#' @aliases as.data.frame,ModelInput-method
setMethod("as.data.frame","ModelInput",as.data.frame.ModelInput)

.update.data.frame <- function(x, y, map, cells, t, ...) {
    ## hidden function to update a data.frame containing dynamic explanatory variables
    ##
    ## Args:
    ##   x: a data.frame
    ##   y: an ExpVarMaps object
    ##   map: ???
    ##   cells: ???
    ##   t: the time for which dynamic explanatory variables should be updated
    ##
    ## Returns:
    ##   a data.frame
    
    ix <- t + 1
    nms <- names(x)
    if (length(y@maps) > 0) {
        dynamic.ix <- which(as.logical(sapply(y@maps, function(x) (nlayers(x) > 1))))
        if (length(dynamic.ix) > 0) {
            s <- raster::stack(lapply(y@maps[dynamic.ix], function(x) x[[ix]]))
            update.vals <- s[cells]
            x[,dynamic.ix] <- update.vals
        }
    }
    
    ## if (length(y@calls) > 0) {
    ##     call.ix <- which(names(x) %in% y@call.names)
    ##     calls <- lapply(y@calls, function(x) PredictorCall(x, update.arg=map))
    ##     s <- raster::stack(lapply(calls, function(x) x@map))
    ##     update.vals <- s[cells]
    ##     x[,call.ix] <- update.vals
    ## }
    names(x) <- nms
    x
}
    
.getExpVarMaps <- function(maps, t) {
    index <- t + 1
    for (i in 1:length(maps)) {
        s <- maps[[i]]
        n <- raster::nlayers(s)
        if (n == 1) {
            maps[[i]] <- s[[1]]
        } else if (index <= n) {
            maps[[i]] <- s[[index]] 
        } else if (index > n) {
            stop("invalid t: dynamic explanatory variables, but no data for this time")
        }
    }
    maps
}


## #' @rdname as.data.frame
## #' @aliases as.data.frame,ExpVarMaps-method
## setMethod("as.data.frame","ExpVarMaps",as.data.frame.ExpVarMaps)

## #' @rdname as.data.frame
## #' @aliases as.data.frame,ModelInput-method
## setMethod("as.data.frame","ModelInput",as.data.frame.ModelInput)

## setGeneric("as.data.frame")

## #' @rdname as.data.frame
## #' @aliases as.data.frame,ExpVarMaps-method
## setMethod("as.data.frame", signature(x = "ExpVarMaps"),
##           function(x, row.names=NULL, optional=FALSE, cells, timestep=0, ...) {
##               ix <- timestep + 1
##               ##maps <- c(.getExpVarMaps(x@maps, timestep), lapply(x@calls, function(x) x@map))
##               maps <- .getExpVarMaps(x@maps, timestep)
##               s <- raster::stack(maps, ...) ## this will fail if map characteristics do not agree
##               df <- as.data.frame(s[cells], row.names=row.names, optional=optional)
##               names(df) <- x@varnames
##               df
##           }
## )

## .update.data.frame <- function(x, y, map, cells, timestep, ...) {
##     ix <- timestep + 1
##     nms <- names(x)
##     if (length(y@maps) > 0) {
##         dynamic.ix <- which(as.logical(sapply(y@maps, function(x) (nlayers(x) > 1))))
##         if (length(dynamic.ix) > 0) {
##             s <- raster::stack(lapply(y@maps[dynamic.ix], function(x) x[[ix]]))
##             update.vals <- s[cells]
##             x[,dynamic.ix] <- update.vals
##         }
##     }
##     ## if (length(y@calls) > 0) {
##     ##     call.ix <- which(names(x) %in% y@call.names)
##     ##     calls <- lapply(y@calls, function(x) PredictorCall(x, update.arg=map))
##     ##     s <- raster::stack(lapply(calls, function(x) x@map))
##     ##     update.vals <- s[cells]
##     ##     x[,call.ix] <- update.vals
##     ## }
##     names(x) <- nms
##     x
## }
    
## .getExpVarMaps <- function(maps, timestep) {
##     index <- timestep + 1
##     for (i in 1:length(maps)) {
##         s <- maps[[i]]
##         n <- raster::nlayers(s)
##         if (n == 1) {
##             maps[[i]] <- s[[1]]
##         } else if (index <= n) {
##             maps[[i]] <- s[[index]] 
##         } else if (index > n) {
##             stop("invalid timestep")
##         }
##     }
##     maps
## }
    
