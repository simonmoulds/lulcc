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
#' ## Plum Island Ecosystems
#'
#' ## Load observed land use maps
#' obs <- ObsLulcMaps(x=pie,
#'                    pattern="lu",
#'                    categories=c(1,2,3),
#'                    labels=c("forest","built","other"),
#'                    t=c(0,6,14))
#'
#' ## create equally sized training and testing partitions
#' part <- partition(x=obs[[1]], size=0.5, spatial=FALSE)

partition <- function(x, size=0.5, spatial=TRUE, ...) {
    points <- raster::rasterToPoints(x, spatial=TRUE)
    cells <- raster::cellFromXY(x, points)
    train.ix <- .createDataPartition(y=points@data[,1], p=size, list=FALSE, times=1)[,1]
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

## createDataPartition function from 'caret' package
.createDataPartition <- function (y, times = 1, p = 0.5, list = TRUE, groups = min(5, length(y)))
{
  out <- vector(mode = "list", times)

  if(length(y) < 2) stop("y must have at least 2 data points")
  
  if(groups < 2) groups <- 2
  
  if(is.numeric(y))
    {
      y <- cut(y, 
               unique(quantile(y, probs = seq(0, 1, length = groups))), 
               include.lowest = TRUE)
    }
  
  y <- factor(y)
  dataInd <- seq(along = y)
  numInClass <- table(y)
  sampleNums <- ceiling(numInClass * p)
  sampleNums <- ifelse(sampleNums == numInClass, sampleNums - 
                       1, sampleNums)
  groupNames <- names(sampleNums)
  for (j in 1:times) {
    for (i in seq(along = sampleNums)) {
      if (sampleNums[i] > 0) {
        trainData <- sort(sample(dataInd[y = which(y == 
                                           groupNames[i])], sampleNums[i]))
        out[[j]] <- append(out[[j]], trainData)
      }
    }
  }
  
  if (!list)
    {
      out <- matrix(unlist(out), ncol = times)
      colnames(out) <- .prettySeq(1:ncol(out))
    } else {
      names(out) <- .prettySeq(out)
    }
  out
}

## prettySeq function from 'caret' package
.prettySeq <- function(x) paste("Resample", gsub(" ", "0", format(seq(along = x))), sep = "")
