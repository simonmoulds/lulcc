#' @include class-PerformanceMulti.R
NULL

if (!isGeneric("plot.roc")) {
    setGeneric("plot.roc", function(perf, ...)
               standardGeneric("plot.roc"))
}

#' Plot methods for PerformanceMulti objects
#'
#' Plot the the ROC curve for each \code{performance} object in a
#' \code{\link{PerformanceMulti}} object. If a list of \code{PerformanceMulti}
#' objects is provided, with each list element representing a different type
#' of mathematical model, ROC curves for different models are included on the
#' same plot for model comparison
#'
#' @param perf either a single \code{PerformanceMulti} object or a list of these.
#'   If a list is provided it must be named.
#' @param multipanel logical. If \code{TRUE}, create a trellis plot where the
#'   number of panels equals the number of \code{PerformanceMulti} objects.
#'   Otherwise, create a single plot for each \code{PerformanceMulti} object
#' @param ... additional arguments to \code{xyplot}
#'
#' @seealso \code{\link{PerformanceMulti}}
#' @author Simon Moulds
#' @return a single plot (if \code{multipanel=FALSE}) or a list of plots.
#'
#' @rdname plot.roc
#'
#' @export

#' @rdname plot.roc
#' @aliases plot.roc,PerformanceMulti-method
setMethod("plot.roc", signature(perf = "PerformanceMulti"),
          function(perf, type, ...) {
            l <- list(perf)
            names(l) <- type
            out <- plot.roc(l, ...)
          }
)

#' @rdname plot.roc
#' @aliases plot.roc,list-method
setMethod("plot.roc", signature(perf = "list"),
          function(perf, multipanel=TRUE, type="l", abline=list(c(0,1), col="grey"), col=RColorBrewer::brewer.pal(9, "Set1"), key.args=NULL, ...) {

          ## function(perf, multipanel=TRUE, type="l", abline=list(c(0,1), col="grey"),
          ##          col=brewer.pal(9,"Set1"), aspect="iso",
          ##          ##xlab="False positive rate (1-specificity)",
          ##          ##ylab="True positive rate (sensitivity)",
          ##          key.args=list(cex=0.8, size=2), ...) {

              c1 <- all(sapply(perf, function(x) is(x, "PerformanceMulti")))
              if (!c1) stop("all objects in list should have class 'PerformanceMulti'")

              categories <- sort(unique(unlist(lapply(perf, function(x) x@categories))))
              out <- list()
              rocdata <- list()
              aucdata <- list()
              
              for (i in 1:length(categories)) {
                  ix <- sapply(perf, function(x) categories[i] %in% x@categories)              
                  roc <- list()
                  stats <- list()
                  for (j in 1:length(ix)) {
                      if (ix[j]) {
                          p <- perf[[j]]
                          ixx <- which(p@categories %in% categories[i])
                          ixxx <- length(roc) + 1
                          roc[[ixxx]] <- data.frame(x=p@performance[[ixx]]@x.values[[1]],
                                                    y=p@performance[[ixx]]@y.values[[1]],
                                                    lab=p@labels[ixx])
                          stats[[ixxx]] <- data.frame(auc=p@auc[ixx])
                      }
                  }
                
                  names(roc) <- names(perf)[ix]
                  names(stats) <- names(perf)[ix]

                  aucdata[[i]] <- plyr::ldply(stats)
                  rocdata[[i]] <- plyr::ldply(roc)
                 
              }

              plotdata <- do.call(rbind, rocdata)
              plotdata$.id <- factor(plotdata$.id, levels=names(perf), order=TRUE)
              
              p <- lattice::xyplot(y ~ x|lab,
                                   data=plotdata,
                                   group=.id,
                                   as.table=TRUE,
                                   type=type,
                                   abline=abline, 
                                   col=col,
                                   ##aspect=aspect, 
                                   ##xlab=xlab,
                                   ##ylab=ylab,
                                   panel=.panel.roc, 
                                   auc=aucdata, 
                                   roc.col=col,
                                   key.args=key.args, ...)
              
          }
)

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

