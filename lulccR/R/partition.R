#' Partition raster data 
#'
#' Divide a categorical raster map into training and testing partitions.
#'
#' @param x RasterLayer with categorical data
#' @param size numeric value between zero and one indicating the proportion of
#'   non-NA cells that should be included in the training partition. Default is
#'   0.5, which results in equally sized partitions
#' @param spatial logical. If TRUE, the function returns a SpatialPoints object
#'   with the coordinates of cells in each partition. If FALSE, the cell numbers
#'   are returned
#' @param \dots additional arguments (none)
#'
#' @return A list containing the following components:
#' \describe{
#'   \item{\code{train}}{a SpatialPoints object or numeric vector indicating the
#'   cells in the training partition}
#'   \item{\code{test}}{a SpatialPoints object or numeric vector indicating the
#'   cells in the testing partition}
#'   \item{\code{all}}{a SpatialPoints object or numeric vector indicating all
#'   non-NA cells in the study region}
#' }
#' 
#' @export
#'
#' @examples
#'
#' obs <- ObservedMaps(x=sibuyan,
#'                     pattern="lu",
#'                     categories=c(1,2,3,4,5),
#'                     labels=c("forest","coconut","grass","rice","other"),
#'                     t=c(0))
#'
#' ## create equally sized training and testing partitions
#' partition(x=obs@@maps[[1]], size=0.5, spatial=FALSE)

partition <- function(x, size=0.5, spatial=TRUE, ...) {
    points <- raster::rasterToPoints(x, spatial=TRUE)
    cells <- raster::cellFromXY(x, points)
    train.ix <- caret::createDataPartition(y=points@data[,1], p=size, list=FALSE, times=1)[,1]
    if (spatial) {
        points <- as(points, "SpatialPoints")
        train <- points[train.ix]
        test <- points[-train.ix]
        all <- points
    } else {
        train <- cells[train.ix]
        test <- cells[-train.ix]
        all <- cells
    }
    out <- list(train=train, test=test, all=all)
}
    
