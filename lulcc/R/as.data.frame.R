#' @include class-ExpVarRasterList.R class-Model.R
NULL

#' Coerce objects to data.frame
#'
#' This function extracts data from all raster objects in
#' \code{\link{ObsLulcRasterStack}} or \code{\link{ExpVarRasterList}} objects
#' for a specified timestep.
#'
#' If x is an ObsLulcRasterStack object the raster corresponding to t is first
#' transformed to a RasterBrick with a boolean layer for each class with
#' \code{raster::\link[raster]{layerize}}.
#'
#' @param x an ExpVarRasterList or ObsLulcRasterStack object
#' @param row.names NULL or a character vector giving the row.names for the
#'   data.frame. Missing values are not allowed
#' @param optional logical. If TRUE, setting row names and converting column
#'   names (to syntactic names: see make.names) is optional
#' @param cells index of cells to be extracted, which may be a
#'   \code{SpatialPoints*} object or a numeric vector representing cell numbers
#'   (see \code{raster::\link[raster]{extract}})
#' @param t numeric indicating the time under consideration
#' @param \dots additional arguments (none)
#'
#' @seealso \code{\link[base]{as.data.frame}}, \code{\link{ObsLulcRasterStack}},
#' \code{\link{ExpVarRasterList}}, \code{\link{partition}}
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
#' obs <- ObsLulcRasterStack(x=pie,
#'                           pattern="lu", 
#'                           categories=c(1,2,3), 
#'                           labels=c("Forest","Built","Other"), 
#'                           t=c(0,6,14))
#' 
#' ## explanatory variables
#' ef <- ExpVarRasterList(x=pie, pattern="ef")
#' 
#' ## separate data into training and testing partitions
#' part <- partition(x=obs[[1]], size=0.1, spatial=TRUE)
#' df1 <- as.data.frame(x=obs, cells=part[["all"]], t=0)
#' df2 <- as.data.frame(x=ef, cells=part[["all"]], t=0)
#'
#' }

#' @rdname as.data.frame
#' @method as.data.frame ExpVarRasterList
#' @export
as.data.frame.ExpVarRasterList <- function(x, row.names=NULL, optional=FALSE, cells, t=0, ...) {
    ix <- t + 1
    ##maps <- c(.getExpVarRasterList(x@maps, t), lapply(x@calls, function(x) x@map))
    maps <- .getExpVarRasterList(x@maps, t)
    df <- as.data.frame(matrix(data=NA, nrow=length(cells), ncol=length(maps)))
    for (i in 1:length(maps)) {
        df[,i] <- extract(maps[[i]], cells, ...)
    }
    
    ## s <- raster::stack(maps, ...) ## this will fail if map characteristics do not agree
    ## df <- as.data.frame(s[cells], row.names=row.names, optional=optional)
    names(df) <- x@names
    df
}

#' @rdname as.data.frame
#' @method as.data.frame ObsLulcRasterStack
#' @export
as.data.frame.ObsLulcRasterStack <- function(x, row.names=NULL, optional=FALSE, cells, t=0, ...) {

    if (!t %in% x@t) {
        print("no observed map at time 't': using setting t = 0 instead")
        t <- 0
    }

    ix <- which(x@t %in% t)

    br <- raster::layerize(x[[ix]])
    names(br) <- x@labels
    df <- as.data.frame(raster::extract(x=br, y=cells))
    df
}
    

#' @rdname as.data.frame
#' @aliases as.data.frame,ExpVarRasterList-method
setMethod("as.data.frame","ExpVarRasterList",as.data.frame.ExpVarRasterList)

#' @rdname as.data.frame
#' @aliases as.data.frame,ObsLulcRasterStack-method
setMethod("as.data.frame","ObsLulcRasterStack",as.data.frame.ObsLulcRasterStack)



.update.data.frame <- function(x, y, map, cells, t, ...) {
    ## hidden function to update a data.frame containing dynamic explanatory variables
    ##
    ## Args:
    ##   x: a data.frame
    ##   y: an ExpVarRasterList object
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

    names(x) <- nms
    x
}
    
.getExpVarRasterList <- function(maps, t) {
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

