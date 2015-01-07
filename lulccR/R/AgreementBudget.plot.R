#' @include class-AgreementBudget.R
NULL

#' Plot method for AgreementBudget objects
#'
#' Plot sources of agreement and disagreement between three maps at multiple
#' resolutions
#'
#' The plot layout is based on work presented in Pontius et al. (2011)
#'
#' @param x object of class \code{AgreementBudget}
#' @param col character. A colour palette. Default is 'Set2' from RColorBrewer
#' @param key list. See \code{lattice::\link[lattice]{xyplot}}
#' @param scales list. See \code{lattice::\link[lattice]{xyplot}}
#' @param xlab character or expression. See \code{lattice::\link[lattice]{xyplot}} 
#' @param ylab character or expression. See \code{lattice::\link[lattice]{xyplot}}
#' @param ... additional arguments to \code{lattice::\link[lattice]{xyplot}}
#'
#' @seealso \code{\link{AgreementBudget}}, \code{lattice::\link[lattice]{xyplot}}
#' @author Simon Moulds
#' @return a trellis object
#'
#' @export
#' @rdname AgreementBudget.plot
#'
#' @references Pontius Jr, R.G., Peethambaram, S., Castella, J.C. (2011).
#' Comparison of three maps at multiple resolutions: a case study of land change
#' simulation in Cho Don District, Vietnam. Annals of the Association of American
#' Geographers 101(1): 45-62.

setGeneric("AgreementBudget.plot", function(x, ...)
           standardGeneric("AgreementBudget.plot"))

#' @rdname AgreementBudget.plot
#' @aliases AgreementBudget.plot,AgreementBudget-method
setMethod("AgreementBudget.plot", signature(x = "AgreementBudget"),
          function(x, col=RColorBrewer::brewer.pal(5, "Set2"), key, scales, xlab, ylab, ...) {
            
              dots <- list(...)
              #cols <- pal #RColorBrewer::brewer.pal(5, pal)
              border.col <- rep("black", 5)
              labels <- c("change simulated as persistence",
                          "change simulated correctly",
                          "change simulated as change to wrong category",
                          "persistence simulated as change",
                          "persistence simulated correctly")

               ## number of sources of agreement and disagreement (correct persistence not possible for 'transition')
               if (x@type == "overall" || x@type == "category") {
                   n <- 5
               } else {
                   n <- 4
                   col <- c(col[1:n], "transparent")
                   border.col <- c(border.col[1:n], "transparent")
                   labels <- c(labels[1:n], "")
               }

               data <- t(apply(x@agreement, 1, cumsum))
               data <- cbind(data.frame(origin=rep(0, nrow(data))), data)
               data.list <- list()

               for (i in 1:n) {
                   var <- names(data)[(i+1)]
                   upper <- data[,(i+1)]
                   lower <- data[,i]
                   data.list[[i]] <- data.frame(x=c(1:length(x@factors), rev(1:length(x@factors))),
                                                var=var,
                                                y=c(upper, rev(lower)))
               }
               data <- do.call(rbind, data.list)

               default.key <- list(space="bottom", rectangles=list(col=col, border=border.col), text=list(labels), reverse.rows=TRUE)
               if (missing(key)) {
                   key <- default.key
               } else if (!is.null(key)) {
                   key <- c(key, default.key[!names(default.key) %in% names(key)])
               }

               default.scales <- list(x=list(at=1:length(x@factors), labels=x@factors))
               if (missing(scales)) {
                   scales <- default.scales
               } else {
                   scales <- c(scales, default.scales[!names(default.scales) %in% names(scales)])
               }

               default.xlab <- list(label="Resolution (multiple of native pixel size)")
               if (missing(xlab)) {
                   xlab <- default.xlab
               } else {
                   xlab <- c(xlab, default.xlab[!names(default.xlab) %in% names(xlab)])
               }

               default.ylab <- list(label="Fraction of study area")
               if (missing(ylab)) {
                   ylab <- default.ylab
               } else {
                   ylab <- c(ylab, default.ylab[!names(default.ylab) %in% names(ylab)])
               }

               lattice::lattice.options(axis.padding=list(numeric=0))

               p <- lattice::xyplot(y~x,
                             data=data,
                             groups=var,
                             key=key,
                             polygon.col=col,
                             panel=function(x,y,polygon.col,...) {
                                 lattice::panel.superpose(x, y, panel.groups=.my.panel.polygon,col=polygon.col,...)
                             },
                             scales=scales,
                             xlab=xlab,
                             ylab=ylab,
                             ...)

          }
)

.my.panel.polygon <- function(x, y, col, ...) {
    lattice::panel.polygon(x, y, col=col, ...)
}
