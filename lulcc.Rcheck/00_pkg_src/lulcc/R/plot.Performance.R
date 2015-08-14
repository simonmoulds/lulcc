#' @include class-PerformanceList.R
NULL

#' Plot method for PerformanceList objects
#'
#' Plot the the ROC curve for each \code{performance} object in a
#' \code{\link{PerformanceList}} object. If more than one
#' \code{PerformanceList} objects are provided ROC curves for the same land use
#' category from different objects are included on the same plot for model
#' comparison.
#'
#' @param x either a single PerformanceList object or a list of these. If a list is
#'   provided it must be named.
#' @param y not used
#' @param multipanel logical. If \code{TRUE}, create a trellis plot where the
#'   number of panels equals the number of \code{PerformanceList} objects.
#'   Otherwise, create a single plot for each PerformanceList object
#' @param type character. See \code{lattice::\link[lattice]{panel.xyplot}}
#' @param abline list. See \code{lattice::\link[lattice]{panel.xyplot}}
#' @param col character. Plotting colour
#' @param key.args list containing additional components to be passed to
#'   the key argument of \cr
#'   \code{lattice::\link[lattice]{xyplot}}
#' @param \dots additional arguments to \code{lattice::\link[lattice]{xyplot}}
#'
#' @seealso \code{\link{PerformanceList}}, \code{lattice::\link[lattice]{xyplot}}
#'
#' @return A trellis object.
#'
#' @export
#' @name plot.PerformanceList
#' @rdname plot.PerformanceList
#'
#' @examples
#'
#' ## see lulcc-package examples

#' @rdname plot.PerformanceList
#' @method plot PerformanceList
#' @export
plot.PerformanceList <- function(x, y, multipanel=TRUE, type="l", abline=list(c(0,1), col="grey"), col=RColorBrewer::brewer.pal(9, "Set1"), key.args=NULL, ...) {

    ## c1 <- all(sapply(x, function(x) is(x, "PerformanceList")))
    ## if (!c1) stop("all objects in list should have class 'PerformanceList'")

    if (is.null(names(x)) || any(nchar(names(x)) == 0)) { ##stop("'x' must be a named list")
        names(x) <- paste0("type", seq(1,length(x)))
    }

    categories <- sort(unique(unlist(lapply(x, function(x) x@categories))))
    out <- list()
    rocdata <- list()
    aucdata <- list()

    for (i in 1:length(categories)) {
        ix <- sapply(x, function(x) categories[i] %in% x@categories)              
        roc <- list()
        stats <- list()
        for (j in 1:length(ix)) {
            if (ix[j]) {
                p <- x[[j]]
                ixx <- which(p@categories %in% categories[i])
                ixxx <- length(roc) + 1
                roc[[ixxx]] <- data.frame(x=p@performance[[ixx]]@x.values[[1]],
                                          y=p@performance[[ixx]]@y.values[[1]],
                                          lab=p@labels[ixx])
                stats[[ixxx]] <- data.frame(auc=p@auc[ixx])
            }
        }

        names(roc) <- names(x)[ix]
        names(stats) <- names(x)[ix]

        aucdata[[i]] <- plyr::ldply(stats)
        rocdata[[i]] <- plyr::ldply(roc)

    }

    .id <- NULL ## avoids NOTE
    plotdata <- do.call(rbind, rocdata)
    plotdata$.id <- factor(plotdata$.id, levels=names(x), ordered=TRUE)

    p <- lattice::xyplot(y ~ x|lab,
                         data=plotdata,
                         group=.id,
                         as.table=TRUE,
                         type=type,
                         abline=abline, 
                         col=col,
                         panel=.panel.roc, 
                         auc=aucdata,
                         roc.col=col,
                         key.args=key.args, ...)

    p

}

#' @rdname plot.PerformanceList
#' @aliases plot,list,ANY-method
setMethod("plot","list",plot.PerformanceList)


.custom.key <- function(key, corner=c(0,1), x=corner[1], y=corner[2]) {
    key.gf <- lattice::draw.key(key, draw=FALSE)
    vp <- grid::viewport(x = grid::unit(x, "npc") + grid::unit(0.5 - corner[1], "grobwidth", list(key.gf)),
                         y = grid::unit(y, "npc") + grid::unit(0.5 - corner[2], "grobheight", list(key.gf)))
    grid::pushViewport(vp)
    grid::grid.draw(key.gf)
    grid::upViewport()
}

.panel.roc <- function(x, y, groups, subscripts, auc, roc.col, key.args, ...) {
    lattice::panel.xyplot(x, y, groups=groups, subscripts=subscripts, ...)
    ix <- lattice::panel.number()
    annotation <- with(auc[[ix]], paste0(.id, ": AUC=", auc))
    key <- list(lines=list(col=roc.col[1:length(auc[[ix]]$.id)]), text=list(annotation))
    key <- c(key, key.args)
    .custom.key(key=key, corner=c(0.98, 0.02))
}



## setGeneric("PerformanceList.plot", function(x, ...)
##            standardGeneric("PerformanceList.plot"))

## ##  @rdname PerformanceList.plot
## ##  @aliases PerformanceList.plot,PerformanceList-method
## ## setMethod("PerformanceList.plot", signature(x = "PerformanceList"),
## ##           function(x, type, ...) {
## ##               l <- list(x)
## ##               out <- PerformanceList.plot(l, ...)
## ##           }
## ## )
