#' Total number of cells in a categorical Raster* object
#' 
#' Count the number of cells belonging to each category in a Raster* object.
#'
#' @param x Raster* object
#' @param categories numeric vector containing land use categories. Only cells
#'   belonging to these categories will be counted
#'
#' @return A list containing the following components:
#' \describe{
#'   \item{\code{total}}{a matrix containing the total number of cells belonging
#'     to each category. Rows represent layers in the input Raster* object}
#'   \item{\code{categories}}{the categories included in the calculation}
#' }
#'
#' @author Simon Moulds
#'
#' @useDynLib lulccR
#'
#' @export
#'
#' @examples
#' # RasterLayer
#' total(x=sib$lu_sib_1997)
#'
#' # RasterStack
#' total(x=stack(pie$lu_pie_1985, pie$lu_pie_1991, pie$lu_pie_1999))

total <- function(x, categories) {
    
    if (missing(x)) stop("missing argument 'x'")
    if (missing(categories)) {
        warning("missing argument 'categories': getting categories from 'x'")
        categories <- sort(unique(as.numeric(raster::getValues(x))))
    }
    
    area <- matrix(data=NA, nrow=raster::nlayers(x), ncol=length(categories))
    for (i in 1:raster::nlayers(x)) {
        vals <- raster::getValues(x[[i]])
        area[i,] <- .Call("total", vals, categories, PACKAGE='lulccR')
    }
    
    out <- list()
    out[["total"]] <- area
    out[["categories"]] <- categories
    out
}
