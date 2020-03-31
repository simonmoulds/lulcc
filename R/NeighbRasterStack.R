#' @include class-NeighbRasterStack.R
NULL

#' Create a NeighbRasterStack object
#'
#' Methods to calculate neighbourhood values for cells in raster maps using
#' \code{raster::\link[raster]{focal}}. By default the fraction of non-NA cells
#' within the moving window (i.e. the size of the weights matrix) devoted to each
#' land use category is calculated. This behaviour can be changed by altering the
#' weights matrix or providing an alternative function. The resulting object can
#' be used as the basis of neighbourhood decision rules.
#'
#' @param x RasterLayer containing categorical data
#' @param categories numeric vector containing land use categories for which
#'   neighbourhood values should be calculated
#' @param weights list containing a matrix of weights (the \code{w} argument in
#'   \code{raster::\link[raster]{focal}}) for each land use category. The order
#'    of list or vector elements should correspond to the order of land use
#'    categories in \code{categories}
#' @param neighb NeighbRasterStack object. Only used if \code{categories} and
#'   \code{weights} are not provided. This option can be useful when existing
#'   NeighbRasterStack objects need to be updated because a new land use map is
#'   available, such as during the allocation procedure.
#' @param fun function. Input argument to \code{focal}. Default is \code{mean}
#' @param \dots additional arguments to \code{raster::\link[raster]{focal}}
#'
#' @seealso \code{\link{NeighbRasterStack-class}}, \code{\link{allowNeighb}},
#' \code{raster::\link[raster]{focal}}
#'
#' @return A NeighbRasterStack object.
#'
#' @export
#' @rdname NeighbRasterStack
#'
#' @examples
#'
#' ## Plum Island Ecosystems
#'
#' ## observed data
#' obs <- ObsLulcRasterStack(x=pie,
#'                     pattern="lu",
#'                     categories=c(1,2,3),
#'                     labels=c("forest","built","other"),
#'                     t=c(0,6,14))
#' 
#' ## create a NeighbRasterStack object for 1985 land use map
#' w1 <- matrix(data=1, nrow=3, ncol=3, byrow=TRUE)
#' w2 <- w1
#' w3 <- w1
#' 
#' nb1 <- NeighbRasterStack(x=obs[[1]],
#'                  categories=c(1,2,3),
#'                  weights=list(w1,w2,w3))
#' 
#' ## update nb2 for 1991
#' nb2 <- NeighbRasterStack(x=obs[[2]],
#'                   neighb=nb1)
#' 
#' ## plot neighbourhood map for forest
#' plot(nb2[[1]])
#'

setGeneric("NeighbRasterStack", function(x, weights, neighb, ...)
           standardGeneric("NeighbRasterStack"))

#' @rdname NeighbRasterStack
#' @aliases NeighbRasterStack,RasterLayer,list,ANY-method
setMethod("NeighbRasterStack", signature(x = "RasterLayer", weights = "list", neighb="ANY"),
          function(x, weights, neighb, categories, fun=mean, ...) {

              if (length(weights) != length(categories)) stop("'weights' and 'categories' must have same length")
                            
              vals <- raster::getValues(x)
              missing.ix <- (!categories %in% vals)

              if (length(which(missing.ix)) > 0) {
                  warning(paste0("categories ", categories[missing.ix], " not found in 'x'"))
              } 

              categories <- categories[!missing.ix]
              weights <- weights[!missing.ix]
              
              maps <- list()
              calls <- list()
    
              for (i in 1:length(categories)) {
                  ## if (categories[i] %in% vals) {
                  r <- (x == categories[i])
                  w <- weights[[i]]
                  cl <- call("focal", x=r, w=w, fun=fun, pad=TRUE, na.rm=TRUE)#, list(...))

                  ##maps[[ix]] <- eval(cl) ##raster::focal(r, weights[[i]], fun=fun, pad=TRUE, na.rm=TRUE, ...)
                  
                  maps[[i]]  <- eval(cl)
                  cl$x <- NA
                  calls[[i]] <- cl
                      
              }

              maps <- stack(maps)
              out <- new("NeighbRasterStack", maps, calls=calls, categories=categories)  
          }
)

#' @rdname NeighbRasterStack
#' @aliases NeighbRasterStack,RasterLayer,matrix,ANY-method
setMethod("NeighbRasterStack", signature(x = "RasterLayer", weights = "matrix", neighb="ANY"),
          function(x, weights, neighb, categories, fun=mean, ...) {
              ##if (length(weights) != length(categories)) stop("'weights' and 'categories' must have same lengths")
              ##if (any(is.na(weights))) stop("'weights' cannot contain NAs")           
              weights.list <- list()
              for (i in 1:length(categories)) {
                  weights.list[[i]] <- weights
              }
              
              out <- NeighbRasterStack(x=x, weights=weights.list, categories=categories, fun=fun, ...)
          }
)

#' @rdname NeighbRasterStack
#' @aliases NeighbRasterStack,RasterLayer,ANY,NeighbRasterStack-method
setMethod("NeighbRasterStack", signature(x = "RasterLayer", weights = "ANY", neighb="NeighbRasterStack"),
          function(x, weights, neighb) {

              categories <- neighb@categories
              calls <- list()
              maps  <- list()
              
              for (i in 1:length(categories)) {
                  r <- (x == categories[i])
                  cl <- neighb@calls[[i]]
                  cl$x <- r
                  maps <- eval(cl)
                  cl$x <- NA  ## set this to NA to save space
                  calls[[i]] <- cl
              }
              maps <- stack(maps)
              out <- new("NeighbRasterStack", maps, calls=calls, categories=categories)  
              
              ##out <- do.call("NeighbRasterStack", c(list(x=x, categories=neighb@categories, weights=neighb@weights, fun=neighb@fun), neighb@focal.args))              
          }
)
