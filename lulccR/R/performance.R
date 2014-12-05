#' @importFrom ROCR performance
NULL

if (!isGeneric("performance")) {
    setGeneric("performance", function(prediction.obj, ...)
               standardGeneric("performance"))
}

#' Create \code{performance} objects
#'
#' A wrapper function for \code{ROCR::\link[ROCR]{performance}} to create
#' \code{performance} objects from a list of \code{prediction} objects.
#'
#' @param prediction.obj a list of \code{prediction} objects 
#' @param ... additional arguments to \code{ROCR::\link[ROCR]{performance}}
#'
#' @seealso \code{ROCR::\link[ROCR]{prediction}},
#' \code{ROCR::\link[ROCR]{performance}}
#' @author Simon Moulds
#' @return a list of \code{performance} objects
#'
#' @rdname performance
#'
#' @export
#' 
#' @references Sing, T., Sander, O., Beerenwinkel, N., Lengauer, T. (2005).
#' ROCR: visualizing classifier performance in R. Bioinformatics
#' 21(20):3940-3941.

#' @rdname performance
#' @aliases performance,list-method
setMethod("performance", signature(prediction.obj = "list"),
          function(prediction.obj, measure, x.measure="cutoff", ...) {
              perf <- list()
              for (i in 1:length(prediction.obj)) {
                  perf[[i]] <- ROCR::performance(prediction.obj[[i]], measure, x.measure, ...)
              }
              perf
          }
)
