#' @include class-Performance.R class-ThreeMapComparison.R class-AgreementBudget.R
NULL

#' Plot method for AgreementBudget objects
#'
#' Plot an \code{\link{AgreementBudget}} object.
#'
#' The plot layout is based on work presented in Pontius et al. (2011)
#'
#' @param x a Performance, AgreementBudget or FigureOfMerit object. A list of
#'   Performance object may also be supplied.
#' @param from optional numeric value representing a land use category. If
#'   provided without \code{to} the agreement budget or figure of merit for all
#'   transitions from this category will be plotted
#' @param to similar to \code{from}. If provided with a valid \code{from}
#'   argument the transition defined by these two arguments (i.e. \code{from} ->
#'   \code{to}) will be plotted
#' @param col character specifying the plotting colour. Default is to use the
#'   'Set2' palette from \code{RColorBrewer}
#' @param type character. See \code{lattice::\link[lattice]{panel.xyplot}}
#' @param abline list. See \code{lattice::\link[lattice]{panel.xyplot}}
#' @param key list. See \code{lattice::\link[lattice]{xyplot}}
#' @param key.args list containing additional components to be passed to
#'   the key argument of \cr
#'   \code{lattice::\link[lattice]{xyplot}}
#' @param scales list. See \code{lattice::\link[lattice]{xyplot}}
#' @param xlab character or expression. See \code{lattice::\link[lattice]{xyplot}} 
#' @param ylab character or expression. See \code{lattice::\link[lattice]{xyplot}}
#' @param multipanel logical. If \code{TRUE}, create a trellis plot where the
#'   number of panels equals the number of \code{Performance} objects.
#'   Otherwise, create a single plot for each Performance object
#' @param \dots additional arguments to \code{lattice::\link[lattice]{xyplot}}
#'
#' @seealso \code{\link{AgreementBudget}}, \code{lattice::\link[lattice]{xyplot}}
#' @return A trellis object.
#'
#' @export
#' @rdname plot
#'
#' @references Pontius Jr, R.G., Peethambaram, S., Castella, J.C. (2011).
#' Comparison of three maps at multiple resolutions: a case study of land change
#' simulation in Cho Don District, Vietnam. Annals of the Association of American
#' Geographers 101(1): 45-62.
#'
#' @examples
#'
#' ## see lulccR-package examples

#' @export
setGeneric("plot")

#' @rdname plot
#' @aliases plot,Performance,ANY-method
setMethod("plot", signature(x = "Performance", y = "ANY"),
          function(x, y, ...) {
              x <- list(x)
              plot(x, ...)
          }
)

#' @rdname plot
#' @aliases plot,list,ANY-method
setMethod("plot", signature(x = "list", y = "ANY"),
          function(x, y, multipanel=TRUE, type="l", abline=list(c(0,1), col="grey"), col=RColorBrewer::brewer.pal(9, "Set1"), key.args=NULL, ...) {

              c1 <- all(sapply(x, function(x) is(x, "Performance")))
              if (!c1) stop("all objects in list should have class 'Performance'")

              if (is.null(names(x)) || any(nchar(names(x)) == 0)) stop("'x' must be a named list")
              
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

              plotdata <- do.call(rbind, rocdata)
              plotdata$.id <- factor(plotdata$.id, levels=names(x), order=TRUE)
              
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
              print(p)
              return(p)
              
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


#' @rdname plot
#' @aliases plot,AgreementBudget,ANY-method
setMethod("plot", signature(x = "AgreementBudget", y = "ANY"),
          function(x, y, from, to, col=RColorBrewer::brewer.pal(5, "Set2"), key, scales, xlab, ylab, ...) {

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
              labels <- c("change simulated as persistence",
                          "change simulated correctly",
                          "change simulated as change to wrong category",
                          "persistence simulated as change",
                          "persistence simulated correctly")

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
              print(p)
              p

          }
)

.my.panel.polygon <- function(x, y, col, ...) {
    lattice::panel.polygon(x, y, col=col, ...)
}

#' @rdname plot
#' @aliases plot,FigureOfMerit,ANY-method
setMethod("plot", signature(x = "FigureOfMerit", y = "ANY"),
          function(x, y, from, to, col=RColorBrewer::brewer.pal(8, "Set2"), type="b", key, scales, xlab, ylab, ...) {

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
                                      lines=list(type=type, pch=1, col=col[1:length(data.list)], size=5),
                                      text=list(sapply(var.list, function(x) x)),
                                      divide=1)
              } else {
                  default.key <- list(space="bottom",
                                      lines=list(type=type, pch=1, col=col[1], size=5),
                                      text=list(var),
                                      divide=1)
              }
                
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

              default.ylab <- list(label="Figure of merit")
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

              p <- lattice::xyplot(y~x,
                                   data=data,
                                   groups=var,
                                   key=key,
                                   #par.settings=list(layout.heights = list(key.top=1.5)), ##,
                                   line.col=col,
                                   panel=function(x,y,line.col,...) {
                                       lattice::panel.xyplot(x,y,type=type,col=line.col,...)
                                   }, 
                                   xlab=xlab,     #"Resolution (multiple of native pixel size)",
                                   ylab=ylab,     #"Figure of merit",
                                   scales=scales, #list(x=list(at=1:length(x@factors), labels=x@factors)),
                                   ...)
              print(p)
              p

          }
)
