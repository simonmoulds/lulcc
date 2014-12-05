#' @include class-NeighbMaps.R
NULL

if (!isGeneric("NeighbMaps")) {
    setGeneric("NeighbMaps", function(x, categories, weights, neighb, ...)
               standardGeneric("NeighbMaps"))
}

#' Create a NeighbMaps object
#'
#' Methods to calculate neighbourhood values for cells in raster maps using
#' \code{raster::\link[raster]{focal}}. By default the fraction of non-NA cells
#' within the moving window (i.e. the size of the weights matrix) devoted to each
#' land use category is calculated. This behaviour can be changed by altering the
#' weights matrix or providing an alternative function. The resulting object can
#' be used as the basis of neighbourhood decision rules or as predictor variables
#' in statistical models.
#'
#' @param x RasterLayer with categorical data
#' @param categories numeric vector containing land use categories for which
#'   neighbourhood values should be calculated
#' @param weights list containing a matrix of weights (the 'w' argument in
#'   \code{focal}) for each land use category or a numeric vector specifying the
#'    size of each weights matrix. In the latter case only square matrices are
#'    possible and all weights are given a value of 1. The order of list or
#'    vector elements should correspond to the order of land use categories in
#'    \code{categories}
#' @param neighb NeighbMaps object. Only used if \code{categories} and
#'   \code{weights} are not provided. This option can be useful when existing
#'   NeighbMaps objects need to be updated because a new land use map is
#'   available, such as during the allocation procedure. In this case
#'   \code{categories} and \code{weights} are set to \code{neighb@@categories}
#'   and \code{neighb@@weights} respectively
#' @param fun function. Input argument to \code{focal}. Default is \code{mean}
#' @param ... additional arguments to \code{focal}
#'
#' @seealso \code{\link{allowNeighb}}
#' @author Simon Moulds
#' @return a NeighbMaps object
#'
#' @rdname NeighbMaps
#'
#' @export
#'
#' @examples
#'
#' ## observed data
#' obs <- ObservedMaps(x=pie,
#'                     pattern="lu",
#'                     categories=c(1,2,3),
#'                     labels=c("forest","built","other"),
#'                     t=c(0,6,14))
#'
#' ## create a NeighbMaps object for 1985 land use map
#' nb1 <- NeighbMaps(x=obs@@maps[[1]],     
#'                   categories=c(1,2,3), # all land use categories
#'                   weights=3)           # 3*3 neighbourhood
#'
#' w1 <- matrix(data=c(1,1,1,
#'                     1,1,1,
#'                     1,1,1), nrow=3, ncol=3, byrow=TRUE)
#'
#' w2 <- matrix(data=c(1,1,1,
#'                     1,1,1,
#'                     1,1,1), nrow=3, ncol=3, byrow=TRUE)
#'
#' w3 <- matrix(data=c(1,1,1,
#'                     1,1,1,
#'                     1,1,1), nrow=3, ncol=3, byrow=TRUE)
#'
#' nb2 <- NeighbMaps(x=obs@@maps[[1]],
#'                   categories=c(1,2,3),
#'                   weights=list(w1,w2,w3))
#'
#' ## update nb2 for 1991
#' nb2 <- NeighbMaps(x=obs@@maps[[2]],
#'                   neighb=nb2)

#' @rdname NeighbMaps
#' @aliases NeighbMaps,RasterLayer,numeric,list,ANY-method
setMethod("NeighbMaps", signature(x = "RasterLayer", categories = "numeric", weights = "list", neighb="ANY"),
          function(x, categories, weights, neighb, fun=mean, ...) {
              if (length(weights) != length(categories)) stop("'weights' and 'categories' must have same length")
              if (any(is.na(weights))) stop("'weights' cannot contain NAs")            
              maps <- list()
              vals <- raster::getValues(x)
              for (i in 1:length(categories)) {
                  if (categories[i] %in% vals) {
                      r <- (x[[i]] == categories[i])
                      ix <- length(maps) + 1
                      maps[[ix]] <- raster::focal(r, weights[[i]], fun=fun, pad=TRUE, na.rm=TRUE, ...)
                  } else {
                      maps[[i]] <- NA
                      warning(paste0("category ", categories[i], " not found in input map"))
                  }
              }
              out <- new("NeighbMaps", maps=maps, weights=weights, fun=fun, focal.args=list(...), categories=categories)  
          }
)

#' @rdname NeighbMaps
#' @aliases NeighbMaps,RasterLayer,numeric,numeric,ANY-method
setMethod("NeighbMaps", signature(x = "RasterLayer", categories = "numeric", weights = "numeric", neighb="ANY"),
          function(x, categories, weights, neighb, fun=mean, ...) {
              if (length(weights) != length(categories)) stop("'weights' and 'categories' must have same lengths")
              if (any(is.na(weights))) stop("'weights' cannot contain NAs")           
              weights.list <- list()
              for (i in 1:length(categories)) {
                  weights.list[[i]] <- matrix(data=1, nrow=weights[i], ncol=weights[i])
              }
              out <- NeighbMaps(x=x, categories=categories, weights=weights.list, fun=fun, ...)
          }
)

#' @rdname NeighbMaps
#' @aliases NeighbMaps,RasterLayer,ANY,ANY,NeighbMaps-method
setMethod("NeighbMaps", signature(x = "RasterLayer", categories = "ANY", weights = "ANY", neighb="NeighbMaps"),
          function(x, categories, weights, neighb) {
              out <- do.call("NeighbMaps", c(list(x=x, categories=neighb@categories, weights=neighb@weights, fun=neighb@fun), neighb@focal.args))              
          }
)
