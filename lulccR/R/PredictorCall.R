#' @include class-PredictorCall.R
NULL

## if (!isGeneric("PredictorCall")) {
##     setGeneric("PredictorCall", function(call, update.arg, name, ...)
##                standardGeneric("PredictorCall"))
## }

#' Create a PredictorCall object 
#'
#' Methods to create a PredictorCall object.
#'
#' @param call an unevaluated function call
#' @param update.arg argument (character) to \code{call} that should be
#'   updated when a new land use map is available during simulation
#' @param name name (character) of the predictor variable
#' @param ... additional arguments (none)
#'
#' @seealso \code{\link{Predictors}}
#' @author Simon Moulds
#' @return PredictorCall object
#'
#' @export
#' @rdname PredictorCall
#'
#' @examples
#' 
#' ## Plum Island Ecosystem
#' obs <- ObservedMaps(x=pie,
#'                     pattern="lu",
#'                     labels=c("forest","built","other"),
#'                     t=c(0,6,14))
#'
#' ## function to calculate distance to land use category
#' myfun <- function(x, category, ...) {
#'    y <- x
#'    y[!is.na(y)] <- 1
#'    x[x != value] <- NA
#'    dist <- distance(x, ...)
#'    dist <- dist * y
#' }
#'
#' ## PredictorCall object for distance to forest
#' dist2forest <- PredictorCall(call=call("myfun", x=obs@@maps[[1]], category=1),
#'                              update.arg="x",
#'                              name="dist2forest")

#if (!isGeneric("PredictorCall")) {
setGeneric("PredictorCall", function(call, update.arg, name, ...)
           standardGeneric("PredictorCall"))
#}

#' @rdname PredictorCall
#' @aliases PredictorCall,call,character,character-method
setMethod("PredictorCall", signature(call = "call", update.arg = "character", name = "character"),
          function(call, update.arg, name, ...) {
              map <- eval(call)
              if (!update.arg %in% names(call)) stop("'update.arg' not present in 'call'")
              out <- new("PredictorCall", map=map, call=call, update.arg=update.arg, name=name)
          }
)

#' @rdname PredictorCall
#' @aliases PredictorCall,PredictorCall,RasterLayer,ANY-method
setMethod("PredictorCall", signature(call = "PredictorCall", update.arg = "RasterLayer", name = "ANY"),
          function(call, update.arg, name, ...) {
              new.call <- call@call
              new.call[[call@update.arg]] <- update.arg
              out <- PredictorCall(call=new.call, update.arg=call@update.arg, name=call@name, ...)
          }
)
          
