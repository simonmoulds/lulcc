#' @include index.R
NULL

#' Cross tabulate land use transitions
#'
#' Cross tabulate land use transitions using
#' \code{raster::\link[raster]{crosstab}}. This step should form the basis of
#' further research into the processes driving the most important transitions in
#' the study region (Pontius et al., 2004).
#'
#' @param x RasterLayer representing land use map from an earlier timestep or an
#'   ObsLulcRasterStack object containing at least two land use maps for different
#'   points in time
#' @param y RasterLayer representing land use map from a later timestep. Not used
#'   if \code{x} is an ObsLulcRasterStack object
#' @param categories numeric vector containing land use categories to consider.
#'   Not used if \code{x} is an ObsLulcRasterStack object
#' @param labels character vector (optional) with labels corresponding to
#'   \code{categories}. Not used if \code{x} is an ObsLulcRasterStack object
#' @param times numeric vector representing the time points of two land use maps
#'   from ObsLulcRasterStack
#' @param \dots additional arguments to \code{raster::\link[raster]{crosstab}}
#'
#' @seealso \code{\link{ObsLulcRasterStack}}, \code{raster::\link[raster]{crosstab}}
#' @return A data.frame.
#'
#' @export
#' @rdname crossTabulate
#' 
#' @references Pontius Jr, R.G., Shusas, E., McEachern, M. (2004). Detecting
#' important categorical land changes while accounting for persistence.
#' Agriculture, Ecosystems & Environment 101(2):251-268.
#'
#' @examples
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
#' crossTabulate(x=obs, times=c(0,14))
#' 
#' ## RasterLayer input
#' crossTabulate(x=obs[[1]],
#'               y=obs[[3]],
#'               categories=c(1,2,3),
#'               labels=c("forest","built","other"))
#' 

setGeneric("crossTabulate", function(x, y, ...)
           standardGeneric("crossTabulate"))

#' @rdname crossTabulate
#' @aliases crossTabulate,RasterLayer,RasterLayer-method
setMethod("crossTabulate", signature(x = "RasterLayer", y = "RasterLayer"),
          function(x, y, categories, labels=as.character(categories), ...) {

              ct <- raster::crosstab(x, y, ...)
              m <- as.data.frame(matrix(data=NA, nrow=length(categories), ncol=length(categories)))
              for (i in 1:length(categories)) {
                  cat1 <- categories[i]
                  for (j in 1:length(categories)) {
                      cat2 <- categories[j]
                      ix <- which(ct$Var1 == cat1 & ct$Var2 == cat2)
                      if (length(ix) == 1) {
                          m[i,j] <- ct$Freq[ix]
                      } else {
                          m[i,j] <- 0
                      }
                      
                  }
              }

              rownames(m) <- labels
              colnames(m) <- labels
                 
              m

          }
)

#' @rdname crossTabulate
#' @aliases crossTabulate,ObsLulcRasterStack,ANY-method
setMethod("crossTabulate", signature(x = "ObsLulcRasterStack", y = "ANY"),
          function(x, y, times, ...) {

              if (nlayers(x) < 2) stop("at least two maps required")
              
              if (missing(times)) {
                  ## index <- c(1,2)
                  t <- x@t[1:2]
              } else if (length(times) != 2) {
                  stop("'times' must contain two time points")
              }  

              if (all(times %in% x@t)) {
                  index <- which(x@t %in% times)
              } else {
                  stop(c("invalid 'times'. Available time points in 'x': ", paste0(x@t, collapse=", ")))
              }

              ix1 <- index[1]
              ix2 <- index[2]
              
              m <- crossTabulate(x=x[[ix1]],
                                 y=x[[ix2]],
                                 categories=x@categories,
                                 labels=x@labels)
              m              
          }
)              
              
