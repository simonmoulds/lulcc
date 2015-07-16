#' @include class-ModelInput.R
NULL

#' Plot method for objects based on Raster* data
#'
#' Plot \code{lulcc} objects based on Raster* data
#'
#' @param x an ObsLulcMaps or *Model object
#' @param y not used
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
#' ## see lulccR-package examples

#' @rdname plot
#' @method plot ObsLulcMaps
#' @export
plot.ObsLulcMaps <- function(x, y, ...) {

    rat <- data.frame(ID=x@categories, labels=x@labels)
    x <- unstack(x)
    for (i in 1:length(x)) {
        levels(x[[i]]) <- rat
    }
    x <- stack(x)
    p <- rasterVis::levelplot(x)#, ...)
    p
}

#' @rdname plot
#' @method plot ModelInput
#' @export
plot.ModelInput <- function(x, y, ...) {

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
    p <- rasterVis::levelplot(output)#, ...)
    p
}

#' @rdname plot
#' @aliases plot,ObsLulcMaps,ANY-method
setMethod("plot", "ObsLulcMaps", plot.ObsLulcMaps)

#' @rdname plot
#' @aliases plot,ModelInput,ANY-method
setMethod("plot", "ModelInput", plot.ModelInput)

