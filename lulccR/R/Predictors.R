#' @include class-Predictors.R
NULL

if (!isGeneric("Predictors")) {
    setGeneric("Predictors", function(maps, calls, ...)
               standardGeneric("Predictors"))
}

#' Create a Predictors object
#'
#' Methods to create a Predictors object.
#'
#' @param maps a list of RasterStack objects such as that returned by
#'   \code{\link{predictorMaps}}
#' @param calls a list of PredictorCall objects
#' @param ... additional arguments (none)
#'
#' @seealso \code{\link{PredictorCall}}, \code{\link{predictorMaps}}
#' @author Simon Moulds
#' @return Predictors object
#'
#' @rdname Predictors
#'
#' @export
#'
#' @examples
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
#' call <- PredictorCall(call=call("myfun", x=obs@@maps[[1]], category=1),
#'                       update.arg="x",
#'                       name="dist2forest")
#'
#' maps <- predictorMaps(x=pie, pattern="pred")
#' pred <- Predictors(maps=maps, calls=list(call))

#' @rdname Predictors
#' @aliases Predictors,missing,list-method
setMethod("Predictors", signature(maps = "missing", calls = "list"),
          function(maps, calls, ...) {
              out <- Predictors(maps = list(), calls=calls, ...)   
          }
)

#' @rdname Predictors
#' @aliases Predictors,list,missing-method
setMethod("Predictors", signature(maps = "list", calls = "missing"),
          function(maps, calls, ...) {
              out <- Predictors(maps=maps, calls=list(), ...)
          }
)

#' @rdname Predictors
#' @aliases Predictors,list,list-method
setMethod("Predictors", signature(maps = "list", calls = "list"),
          function(maps, calls, ...) {
              if (length(maps) == 0 && length(calls) == 0) stop("no predictors found")
              if (length(maps) > 0 && !all(sapply(maps, function(x) is(x, "RasterStack")))) stop("maps must be RasterStack objects")
              if (length(calls) > 0 && !all(sapply(calls, function(x) is(x, "PredictorCall")))) stop("calls must be PredictorCall objects")
              dynamic <- FALSE
              if (length(calls) > 0 || max(sapply(maps, nlayers)) > 1) dynamic <- TRUE
              map.names <- as.character(names(maps))
              call.names <- as.character(sapply(calls, function(x) x@name))             
              out <- new("Predictors",
                         maps=maps,
                         calls=calls,
                         map.names=map.names,
                         call.names=call.names,
                         dynamic=dynamic)
          }
)
