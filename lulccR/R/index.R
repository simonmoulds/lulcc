#' @include class-ExpVarMaps.R class-CategoryLabel.R
NULL

#' Extract by index
#'
#' \code{object[[i]]} can be used to extract individual objects from container
#' classes such as \code{ExpVarMaps}, \code{PredModels}, \code{Prediction} and
#' \code{Performance}.
#'
#' @param x an object of class ExpVarMaps or any object inheriting from the
#'   virtual class CategoryLabel
#' @param i layer number (if 'x' inherits from a RasterStack) or list index (if
#'   'x' stores data as a list)
#' @param j numeric (not used)
#' @param ... additional arguments (none)
#'
#' @export
#' @name Extract by index
#' @rdname extractIndex
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
#' summary(obs[[1]])
#' summary(obs[[1:2]])
#'

#' @rdname extractIndex
#' @aliases [[,ExpVarMaps,ANY,ANY-method
setMethod("[[", "ExpVarMaps",
          function(x,i,j,...) {
              
	      if ( missing(i)) { 
                  stop('you must provide an index') 
	      }
              
 	      if (! missing(j)) { 
	          warning('second index is ignored') 
	      }
              
	      if (is.numeric(i)) {
	          sgn <- sign(i)
		  sgn[sgn==0] <- 1
		  if (! all(sgn == 1) ) {
                      if (! all(sgn == -1) ) {
                          stop("only 0's may be mixed with negative subscripts")
                      } else {
                          i <- (1:length(x))[i]
                      }
                  }
              }
              subset(x, i)
          }
          )

#' @rdname extractIndex
#' @aliases [[,CategoryLabel,ANY,ANY-method
setMethod("[[", "CategoryLabel",
          function(x,i,j,...) {
	      if ( missing(i)) { 
                  stop('you must provide an index') 
	      }
              
 	      if (! missing(j)) { 
	          warning('second index is ignored') 
	      }
              
	      if (is.numeric(i)) {
	          sgn <- sign(i)
		  sgn[sgn==0] <- 1
		  if (! all(sgn == 1) ) {
                      if (! all(sgn == -1) ) {
                          stop("only 0's may be mixed with negative subscripts")
                      } else {
                          i <- (1:length(x))[i]
                      }
                  }
              }
              subset(x, i)
          }
          )
          
