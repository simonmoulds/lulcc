#' @include length.R names.R ExpVarMaps.R Performance.R
NULL

#' Subset
#'
#' TODO
#'
#' @param x TODO
#' @param subset TODO
#' @param ... additional arguments
#'
#' @return TODO
#'
#' @export
#' @rdname subset-methods
#'
#' @examples
#'
#' ## TODO

## setGeneric("subset")

#' @rdname subset-methods
#' @aliases subset,ExpVarMaps-method
setMethod("subset", signature(x="ExpVarMaps"), 
          function(x, subset, ...) {
              subset <- .getsubset(x, subset)
              ## if (is.character(subset)) {
              ##     i <- na.omit(match(subset, names(x@maps)))
              ##     if (length(i)==0) {
              ##         stop('invalid layer names')
              ##     } else if (length(i) < length(subset)) {
              ##         warning('invalid layer names omitted')
              ##     }
              ##     subset <- i
              ## }
              ## subset <- as.integer(subset)
              ## if (! all(subset %in% 1:length(x@maps))) {
              ##     stop('not a valid subset')
              ## }
              if (length(subset) == 1) {
                  x <- ExpVarMaps(x=x@maps[[subset]])
              } else {
                  x <- ExpVarMaps(x@maps[subset])
              }
              return(x)	
          }
          )

#' @rdname subset-methods
#' @aliases subset,PredModels-method
setMethod("subset", signature(x="PredModels"), 
          function(x, subset, ...) {
              subset <- .getsubset(x, subset)
              if (length(subset) == 1) {
                  x <- new("PredModels",
                           models=list(x@models[[subset]]),
                           categories=x@categories[subset],
                           labels=x@labels[subset])
              } else {
                  x <- new("PredModels",
                           models=x@models[subset],
                           categories=x@categories[subset],
                           labels=x@labels[subset])
              }
              return(x)	
          }
          )

#' @rdname subset-methods
#' @aliases subset,Performance-method
setMethod("subset", signature(x="Performance"), 
          function(x, subset, ...) {
              subset <- .getsubset(x, subset)
              ## if (is.character(subset)) {
              ##     i <- na.omit(match(subset, names(x)))
              ##     if (length(i)==0) {
              ##         stop('invalid performance object names')
              ##     } else if (length(i) < length(subset)) {
              ##         warning('invalid performance object names omitted')
              ##     }
              ##     subset <- i
              ## }
              ## subset <- as.integer(subset)
              ## if (! all(subset %in% 1:length(x))) {
              ##     stop('not a valid subset')
              ## }
              if (length(subset) == 1) {
                  x <- new("Performance",
                           performance=list(x@performance[[subset]]),
                           auc=x@auc[subset],
                           categories=x@categories[subset],
                           labels=x@labels[subset])
              } else {
                  x <- new("Performance",
                           performance=x@performance[subset],
                           auc=x@auc[subset],
                           categories=x@categories[subset],
                           labels=x@labels[subset])
              }
              return(x)	
          }
          )

#' @rdname subset-methods
#' @aliases subset,Prediction-method
setMethod("subset", signature(x="Prediction"), 
          function(x, subset, ...) {
              subset <- .getsubset(x, subset)
              ## if (is.character(subset)) {
              ##     i <- na.omit(match(subset, names(x)))
              ##     if (length(i)==0) {
              ##         stop('invalid prediction object names')
              ##     } else if (length(i) < length(subset)) {
              ##         warning('invalid prediction object names omitted')
              ##     }
              ##     subset <- i
              ## }
              ## subset <- as.integer(subset)
              ## if (! all(subset %in% 1:length(x))) {
              ##     stop('not a valid subset')
              ## }
              if (length(subset) == 1) {
                  x <- new("Prediction",
                           prediction=list(x@prediction[[subset]]),
                           categories=x@categories[subset],
                           labels=x@labels[subset])
              } else {
                  x <- new("Prediction",
                           prediction=x@prediction[subset],
                           categories=x@categories[subset],
                           labels=x@labels[subset])
              }
              return(x)	
          }
          )


.getsubset <- function(x, subset) {
    if (is.character(subset)) {
        i <- na.omit(match(subset, names(x)))
        if (length(i)==0) {
            stop('invalid object names')
        } else if (length(i) < length(subset)) {
            warning('invalid object names omitted')
        }
        subset <- i
    }
    subset <- as.integer(subset)
    if (! all(subset %in% 1:length(x))) {
        stop('not a valid subset')
    }
    subset
}
