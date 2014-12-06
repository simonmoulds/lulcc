#' @include class-AgreementBudget.R
NULL

if (!isGeneric("plot.agreement")) {
    setGeneric("plot.agreement", function(x, ...)
               standardGeneric("plot.agreement"))
}

#' Plot method for AgreementBudget objects
#'
#' Plot sources of agreement and disagreement between three maps at multiple
#' resolutions
#'
#' The plot layout is based on work presented in Pontius et al. (2011)
#'
#' @param x object of class \code{AgreementBudget}
#' @param pal character. A colour palette. Default is 'Set2' from RColorBrewer
#' @param ... additional arguments to \code{xyplot}
#'
#' @seealso \code{\link{AgreementBudget}}
#' @author Simon Moulds
#' @return a trellis object
#'
#' @rdname plot.agreement
#'
#' @export
#'
#' @references Pontius Jr, R.G., Peethambaram, S., Castella, J.C. (2011).
#' Comparison of three maps at multiple resolutions: a case study of land change
#' simulation in Cho Don District, Vietnam. Annals of the Association of American
#' Geographers 101(1): 45-62.

#' @rdname plot.agreement
#' @aliases plot.agreement,AgreementBudget-method
setMethod("plot.agreement", signature(x = "AgreementBudget"),
          function(x, pal="Set2", key, scales, xlab, ylab, ...) {
            
              cols <- RColorBrewer::brewer.pal(5, pal)
              border.cols <- rep("black", 5)
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
                   cols <- c(cols[1:n], "transparent")
                   border.cols <- c(border.cols[1:n], "transparent")
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

               default.key <- list(space="bottom", rectangles=list(col=cols, border=border.cols), text=list(labels), reverse.rows=TRUE)
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
                             polygon.cols=cols,
                             panel=function(x,y,polygon.cols,...) {
                                 lattice::panel.superpose(x, y, panel.groups=.my.panel.polygon,col=polygon.cols,...)
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
