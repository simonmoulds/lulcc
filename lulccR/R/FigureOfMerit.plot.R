#' @include class-FigureOfMerit.R
NULL

#' Plot method for FigureOfMerit objects
#'
#' Plot the overall, category-specific or transition-specific figure of merit at
#' different resolutions
#'
#' @param x a \code{FigureOfMerit} object
#' @param from numeric (optional). A single value corresponding to a land use
#'   category. If provided without 'to' the figure of merit for all transitions
#'   from this category will be plotted
#' @param to numeric (optional). Similar to 'from'. If provided with a valid
#'   'from' argument the figure of merit for the transition defined by these two
#'   arguments (i.e. from -> to) will be plotted. It is possible to include more
#'   than one category in which case the different transitions will be included
#'   on the same plot
#' @param type character. See \code{lattice::\link[lattice]{xyplot}}
#' @param key list. See \code{lattice::\link[lattice]{xyplot}}
#' @param scales list. See \code{lattice::\link[lattice]{xyplot}}
#' @param xlab character or expression. See \code{lattice::\link[lattice]{xyplot}} 
#' @param ylab character or expression. See \code{lattice::\link[lattice]{xyplot}}
#' @param ... additional arguments to \code{xyplot}
#'
#' @seealso \code{\link{FigureOfMerit}}
#' @author Simon Moulds
#' @return a trellis object
#'
#' @export
#' @rdname FigureOfMerit.plot

setGeneric("FigureOfMerit.plot", function(x, ...)
           standardGeneric("FigureOfMerit.plot"))

#' @rdname FigureOfMerit.plot
#' @aliases FigureOfMerit.plot,FigureOfMerit-method
setMethod("FigureOfMerit.plot", signature(x = "FigureOfMerit"),
          function(x, from, to, type="b", key, scales, xlab, ylab, ...) {

              if (!missing(from) && missing(to)) fom <- "category"

              if (!missing(from)) {
                  from.ix <- which(x@categories %in% from)
                  if (length(from.ix) > 1) {
                      stop("'from' must be a single land use category")
                  } else if (length(from.ix) == 0) {
                      stop("'from' is not a valid land use category")
                  }

              } else {
                  fom <- "overall"
              }

              if (!missing(from) && !missing(to)) {
                  to.ix <- which(x@categories %in% to)
                  if (length(to.ix) == 0) {
                      stop("'to' is not a valid land use category")
                  }
                  if (length(to.ix) == 1 && from.ix == to.ix) stop("'from' cannot equal 'to'")
                  fom <- "transition"        
              }

              cols <- RColorBrewer::brewer.pal(8, "Set2")

              ## prepare data.frame
              if (fom == "transition") {
                  if (length(to.ix) > 1) {
                      data.list <- list()
                      var.list <- list() ## for key
                      for (i in 1:length(to.ix)) {
                          figureofmerit <- sapply(x@transition, function(x) x[from.ix, to.ix[i]])
                          if (!all(is.na(figureofmerit))) {
                              var <- paste0("transition ", x@labels[from.ix], " -> ", x@labels[to.ix[i]])
                              var.list[[(length(var.list) + 1)]] <- var
                              data.list[[(length(data.list) + 1)]] <- data.frame(x=1:length(x@factors),
                                                                                 var=var,
                                                                                 y=figureofmerit)
                          } 
                      }

                      data <- do.call(rbind, data.list)

                 } else {
                      var <- paste0("transition ", x@labels[from.ix], " -> ", x@labels[to.ix])
                      figureofmerit <- sapply(x@transition, function(x) x[from.ix, to.ix])
                      data <- data.frame(x=1:length(x@factors),
                                         var=var,
                                         y=figureofmerit)

                 }

              } else if (fom == "category") {
                  var <- paste0("transitions from ", x@labels[from.ix])
                  figureofmerit <- sapply(x@category, function(x) x[from.ix])
                  data <- data.frame(x=1:length(x@factors),
                                     var=var,
                                     y=figureofmerit)

              } else if (fom == "overall") {
                  figureofmerit <- sapply(x@overall, function(x) x)
                  var <- paste0("overall")
                  data <- data.frame(x=1:length(x@factors),
                                     var=var,
                                     y=figureofmerit)
              } 

              if (type == "transition" && length(to.ix) > 1) {
                  default.key <- list(space="bottom",
                                      lines=list(type=type, pch=1, col=cols[1:length(data.list)], size=5),
                                      text=list(sapply(var.list, function(x) x)),
                                      divide=1)
              } else {
                  default.key <- list(space="bottom",
                                      lines=list(type=type, pch=1, col=cols[1], size=5),
                                      text=list(var),
                                      divide=1)
              }

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

              default.ylab <- list(label="Figure of merit")
              if (missing(ylab)) {
                  ylab <- default.ylab
              } else {
                  ylab <- c(ylab, default.ylab[!names(default.ylab) %in% names(ylab)])
              }

              p <- lattice::xyplot(y~x,
                                   data=data,
                                   groups=var,
                                   key=key,
                                   #par.settings=list(layout.heights = list(key.top=1.5)), ##,
                                   line.cols=cols,
                                   panel=function(x,y,line.cols,...) {
                                       lattice::panel.xyplot(x,y,type=type,col=line.cols,...)
                                   }, 
                                   xlab=xlab,     #"Resolution (multiple of native pixel size)",
                                   ylab=ylab,     #"Figure of merit",
                                   scales=scales, #list(x=list(at=1:length(x@factors), labels=x@factors)),
                                   ...)

          }
)
