if ( !isGeneric("crossTabulate")) {
    setGeneric("crossTabulate", function(x, y, ...)
               standardGeneric("crossTabulate"))
}

#' Cross tabulate land use transitions
#'
#' Cross tabulate land use transitions using
#' \code{raster::\link[raster]{crosstab}}. This should be the basis for further
#' research into the processes driving the most important transitions in the
#' study region (Pontius et al., 2004).
#'
#' @param x RasterLayer representing land use map from an earlier timestep or an
#'   ObservedMaps object containing at least two land use maps for different
#'   points in time
#' @param y RasterLayer representing land use map from a later timestep. Not used
#'   if \code{x} is an ObservedMaps object
#' @param categories numeric vector containing land use categories to consider.
#'   Not used if \code{x} is an ObservedMaps object
#' @param labels character vector (optional) with labels corresponding to
#'   \code{categories}. Not used if \code{x} is an ObservedMaps object
#' @param index numeric vector with index of land use maps from ObservedMaps
#'   to use in analysis
#' @param ... additional arguments to \code{raster::\link[raster]{crosstab}}
#'
#' @seealso \code{\link{ObservedMaps}}, \code{raster::\link[raster]{crosstab}}
#' @author Simon Moulds
#' @return a data.frame
#'
#' @rdname crossTabulate
#'
#' @export
#' 
#' @references Pontius Jr, R.G., Shusas, E., McEachern, M. (2004). Detecting
#' important categorical land changes while accounting for persistence.
#' Agriculture, Ecosystems & Environment 101(2):251-268.
#'
#' @examples
#'
#' # RasterLayer input
#' crossTabulate(x=pie$lu_pie_1985,
#'               y=pie$lu_pie_1999,
#'               categories=c(1,2,3),
#'               labels=c("forest","built","other"))
#'
#' # ObservedMaps input
#' obs <- ObservedMaps(x=pie,
#'                     pattern="lu",
#'                     categories=c(1,2,3),
#'                     labels=c("forest","built","other"),
#'                     t=c(0,6,14))
#'
#' crossTabulate(x=obs, index=c(1,3))

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
#' @aliases crossTabulate,ObservedMaps,ANY-method
setMethod("crossTabulate", signature(x = "ObservedMaps", y = "ANY"),
          function(x, y, index, ...) {

              if (nlayers(x@maps) < 2) stop("at least two maps required")
              
              if (missing(index)) {
                  index <- c(1,2)
              } else if (length(index) != 2) {
                  stop("index must contain two elements")
              } else if (any(index > raster::nlayers(x@maps))) {
                  stop("index out of range")
              } 

              ix1 <- index[1]
              ix2 <- index[2]
              
              m <- crossTabulate(x=x@maps[[ix1]],
                                 y=x@maps[[ix2]],
                                 categories=x@categories,
                                 labels=x@labels)
              m              
          }
)

              

              
              
