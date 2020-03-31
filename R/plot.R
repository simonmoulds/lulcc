#' @include class-Model.R
NULL

#' Plot method for objects based on Raster* data
#'
#' Plot \code{lulcc} objects based on Raster* data
#'
#' @param x an object from \code{lulcc} containing Raster data
#' @param y not used
#' @param category numeric
#' @param factors numeric
#' @param \dots additional arguments to
#'   \code{rasterVis::\link[rasterVis]{levelplot}}
#'
#' @seealso \code{rasterVis::\link[rasterVis]{levelplot}}
#' @return A trellis object.
#'
#' @export
#' @name plot
#' @rdname plot
#'
#' @examples
#'
#' ## see lulcc-package examples

#' @rdname plot
#' @method plot ObsLulcRasterStack
#' @export
plot.ObsLulcRasterStack <- function(x, y, ...) {

    rat <- data.frame(ID=x@categories, labels=x@labels)
    x <- unstack(x)
    for (i in 1:length(x)) {
        levels(x[[i]]) <- rat
    }
    x <- stack(x)
    p <- rasterVis::levelplot(x, ...)
    p
}

#' @rdname plot
#' @method plot Model
#' @export
plot.Model <- function(x, y, ...) {

    output <- x@output
    if (!is(output, "RasterStack")) {
        stop("'x' does not contain output maps")
    }

    rat <- data.frame(ID=x@categories, labels=x@labels)
    output <- unstack(output)
    for (i in 1:length(output)) {
        levels(output[[i]]) <- rat
    }
    output <- stack(output)
    names(output) <- paste0("t", x@time)
    p <- rasterVis::levelplot(output, ...)
    p
}

#' @rdname plot
#' @method plot ThreeMapComparison
#' @export
plot.ThreeMapComparison <- function(x, y, category, factors, ...) {

    ref.t0 <- x@maps[[1]][[1]]
    ref.t1 <- x@maps[[1]][[2]]
    sim.t1 <- x@maps[[1]][[3]]

    dots <- list(...)    
    if (missing(category)) {
        maps <- stack(ref.t0,ref.t1,sim.t1) 
        rat <- data.frame(ID=x@categories, labels=x@labels)
        maps <- unstack(maps)
        for (i in 1:length(maps)) {
            levels(maps[[i]]) <- rat
        }

        maps <- stack(maps)
        if ("names.attr" %in% names(dots)) {
            p <- rasterVis::levelplot(maps, ...)
        } else {
            nms <- c(paste0(names(ref.t0), " (ref t0)"),
                     paste0(names(ref.t1), " (ref t1)"),
                     paste0(names(sim.t1), " (sim t1)"))
            p <- rasterVis::levelplot(maps, names.attr=nms, ...)
        }   
        
    } else {
        
        if (length(category) > 1) {
            stop("only one category can be supplied")
        }

        if (!category %in% x@categories) {
            stop("invalid category")
        }
        
        lab <- x@labels[x@categories %in% category]

        if (missing(factors)) {
            factors <- x@factors
        }

        if (!all(factors %in% x@factors)) {
            stop("invalid factors")
        }

        plots <- list()
        nms <- list()
        for (i in 1:length(factors)) {
            ix <- which(x@factors %in% factors[i]) + 1
            st <- x@maps[[ix]]
            ix1 <- which(x@categories %in% category)
            start.ix <- (ix1 - 1) * 3 + 1
            end.ix   <- ix1 * 3
            st <- st[[start.ix:end.ix]]

            nms[[i]] <- c(paste0("ref t0: ", lab, " (", factors[i], ")"),
                          paste0("ref t1: ", lab, " (", factors[i], ")"),
                          paste0("sim t1: ", lab, " (", factors[i], ")"))
            
            p <- rasterVis::levelplot(st, layout=c(3,1), ...)
            plots[[i]] <- p
        }
        p <- do.call("c", c(plots, x.same=TRUE, y.same=TRUE))
        p <- update(p, strip=lattice::strip.custom(factor.levels=unlist(nms)), layout=c(3, length(factors)))  ## do this because for some reason strip names are not carried over when using c()
        
    }
    p
}

#' @rdname plot
#' @aliases plot,ObsLulcRasterStack,ANY-method
setMethod("plot", "ObsLulcRasterStack", plot.ObsLulcRasterStack)

#' @rdname plot
#' @aliases plot,Model,ANY-method
setMethod("plot", "Model", plot.Model)

#' @rdname plot
#' @aliases plot,ThreeMapComparison,ANY-method
setMethod("plot", "ThreeMapComparison", plot.ThreeMapComparison)
