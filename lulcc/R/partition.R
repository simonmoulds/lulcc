#' Partition raster data 
#'
#' Divide a categorical raster map into training and testing partitions.
#' A wrapper function for \cr
#' \code{caret::\link[caret]{createDataPartition}} (Kuhn, 2008) to divide a
#' categorical raster map into training and testing partitions.
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
#' @seealso \code{caret::\link[caret]{createDataPartition}}
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
#' @references Kuhn, M. (2008). Building predictive models in R using the caret
#' package. Journal of Statistical Software, 28(5), 1-26.
#'
#' @examples
#'
#' \dontrun{
#' 
#' ## Plum Island Ecosystems
#'
#' ## Load observed land use maps
#' obs <- ObsLulcRasterStack(x=pie,
#'                    pattern="lu",
#'                    categories=c(1,2,3),
#'                    labels=c("forest","built","other"),
#'                    t=c(0,6,14))
#' 
#' ## create equally sized training and testing partitions
#' part <- partition(x=obs[[1]], size=0.1, spatial=FALSE)
#' names(part)
#' 
#' }

partition <- function(x, size=0.5, spatial=TRUE, ...) {
    points <- raster::rasterToPoints(x, spatial=TRUE)
    cells <- raster::cellFromXY(x, points)
    train.ix <- caret::createDataPartition(y=points@data[,1], p=size, list=FALSE, times=1)[,1]
    if (spatial) {
        points <- as(points, "SpatialPoints")
        ## if (size == 1) {
        ##     train <- points
        ##     test <- points
        ## } else {
        train <- points[train.ix]
        test <- points[-train.ix]
        ## }
        all <- points
    } else {
        ## if (size == 1) {
        ##     train <- cells
        ##     test <- cells
        ## } else {
        train <- cells[train.ix]
        test <- cells[-train.ix]
        ## }
        all <- cells
    }
    out <- list(train=train, test=test, all=all)
}
