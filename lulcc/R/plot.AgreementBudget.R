#' @include class-ThreeMapComparison.R
NULL

#' Plot method for AgreementBudget objects
#'
#' Plot an \code{\link{AgreementBudget}} object.
#'
#' The plot layout is based on work presented in Pontius et al. (2011)
#'
#' @param x an AgreementBudget object
#' @param y not used
#' @param from optional numeric value representing a land use category. If 
#'   provided without \code{to} the figure of merit for all transitions from this
#'   category will be plotted
#' @param to similar to \code{from}. If provided with a valid \code{from}
#'   argument the transition defined by these two arguments (i.e. \code{from} ->
#'   \code{to}) will be plotted
#' @param col character specifying the plotting colour. Default is to use the
#'   'Set2' palette from \code{RColorBrewer}
#' @param key list. See \code{lattice::\link[lattice]{xyplot}}
#' @param scales list. See \code{lattice::\link[lattice]{xyplot}}
#' @param xlab character or expression. See \code{lattice::\link[lattice]{xyplot}} 
#' @param ylab character or expression. See \code{lattice::\link[lattice]{xyplot}}
#' @param \dots additional arguments to \code{lattice::\link[lattice]{xyplot}}
#'
#' @seealso \code{\link{AgreementBudget}}, \code{lattice::\link[lattice]{xyplot}}
#' @return A trellis object.
#'
#' @export
#' @name plot.AgreementBudget
#' @rdname plot.AgreementBudget
#'
#' @references Pontius Jr, R.G., Peethambaram, S., Castella, J.C. (2011).
#' Comparison of three maps at multiple resolutions: a case study of land change
#' simulation in Cho Don District, Vietnam. Annals of the Association of American
#' Geographers 101(1): 45-62.
#'
#' @examples
#'
#' ## see lulcc-package examples

#' @rdname plot.AgreementBudget
#' @method plot AgreementBudget
#' @export
plot.AgreementBudget <- function(x, y, from, to, col=RColorBrewer::brewer.pal(5, "Set2"), key, scales, xlab, ylab, ...) {

    if (!missing(from) && missing(to)) type <- "category"

    if (!missing(from)) {
        from.ix <- which(x@categories %in% from)
        if (length(from.ix) > 1) {
            stop("'from' must be a single land use category")
        } else if (length(from.ix) == 0) {
            stop("'from' is not a valid land use category")
        }

    } else {
        type <- "overall"
    }

    if (!missing(from) && !missing(to)) {
        to.ix <- which(x@categories %in% to)
        if (length(to.ix) > 1) {
            stop("'to' must be a single land use category")
        } else if (length(to.ix) == 0) {
            stop("'to' is not a valid land use category")
        }
        if (from.ix == to.ix) stop("'from' cannot equal 'to'")
        type <- "transition"        
    } 

    if (type %in% c("overall"))     agreement <- x@overall
    if (type %in% c("category"))    agreement <- x@category[[from.ix]]
    if (type %in% c("transition"))  agreement <- x@transition[[ ((from.ix - 1) * length(x@categories) + to.ix) ]]

    dots <- list(...)
    border.col <- rep("black", 5)
    labels <- c("Change simulated as persistence (misses)",
                "Change simulated correctly (hits)",
                "Change simulated as change to wrong category (wrong hits)",
                "Persistence simulated as change (false alarms)",
                "Persistence simulated correctly (correct rejections)")


    ## number of sources of agreement and disagreement (correct persistence not possible for 'transition')
    if (type == "overall" || type == "category") {
        n <- 5
    } else {
        n <- 4
        col <- c(col[1:n], "transparent")
        border.col <- c(border.col[1:n], "transparent")
        labels <- c(labels[1:n], "")
    }

    data <- t(apply(agreement, 1, cumsum))
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
        matching.args <- names(default.key)[names(default.key) %in% names(key)]
        key <- c(key, default.key[!names(default.key) %in% names(key)])

        ## deal with embedded lists 
        for (arg in matching.args) {
            key[[arg]] <- c(key[[arg]], default.key[[arg]][!names(default.key[[arg]]) %in% names(key[[arg]])])
        }
    }

    default.scales <- list(x=list(at=1:length(x@factors), labels=x@factors))
    if (missing(scales)) {
        scales <- default.scales
    } else {
        matching.args <- names(default.scales)[names(default.scales) %in% names(scales)]
        scales <- c(scales, default.scales[!names(default.scales) %in% names(scales)])

        ## deal with embedded lists 
        for (arg in matching.args) {
            scales[[arg]] <- c(scales[[arg]], default.scales[[arg]][!names(default.scales[[arg]]) %in% names(scales[[arg]])])
        }

    }

    default.xlab <- list(label="Resolution (multiple of native pixel size)")
    if (missing(xlab)) {
        xlab <- default.xlab
    } else {
        matching.args <- names(default.xlab)[names(default.xlab) %in% names(xlab)]
        xlab <- c(xlab, default.xlab[!names(default.xlab) %in% names(xlab)])

        ## deal with embedded lists 
        for (arg in matching.args) {
            xlab[[arg]] <- c(xlab[[arg]], default.xlab[[arg]][!names(default.xlab[[arg]]) %in% names(xlab[[arg]])])
        }

    }

    default.ylab <- list(label="Fraction of study area")
    if (missing(ylab)) {
        ylab <- default.ylab
    } else {
        matching.args <- names(default.ylab)[names(default.ylab) %in% names(ylab)]
        ylab <- c(ylab, default.ylab[!names(default.ylab) %in% names(ylab)])

        ## deal with embedded lists 
        for (arg in matching.args) {
            ylab[[arg]] <- c(ylab[[arg]], default.ylab[[arg]][!names(default.ylab[[arg]]) %in% names(ylab[[arg]])])
        }

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
    p    
}

#' @rdname plot.AgreementBudget
#' @aliases plot,AgreementBudget,ANY-method
setMethod("plot", "AgreementBudget", plot.AgreementBudget)

.my.panel.polygon <- function(x, y, col, ...) {
    lattice::panel.polygon(x, y, col=col, ...)
}


