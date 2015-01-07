#' @include class-PerformanceMulti.R
NULL

#' Create a PerformanceMulti object
#'
#' This function uses different measures to evaluate multiple
#' \code{ROCR::\link[ROCR]{prediction}} objects stored in a
#' \code{\link{PredictionMulti}} object.
#'
#' @param pred an object of class \code{PredictionMulti}
#' @param measure performance measure to use for the evaluation. See
#'   \code{ROCR::\link[ROCR]{performance}}
#' @param x.measure a second performance measure. See
#'   \code{ROCR::\link[ROCR]{performance}}
#' @param ... additional arguments to \code{\link{performance}}
#' 
#' @seealso \code{\link{performance}}, \code{\link{PredictionMulti}}
#' @author Simon Moulds
#' @return a \code{PerformanceMulti} object
#'
#' @export
#'
#' @references Sing, T., Sander, O., Beerenwinkel, N., Lengauer, T. (2005).
#' ROCR: visualizing classifier performance in R. Bioinformatics
#' 21(20):3940-3941.

PerformanceMulti <- function(pred, measure, x.measure="cutoff", ...) {
    perf <- performance(pred@prediction, measure, x.measure, ...)
    auc <- performance(pred@prediction, measure="auc")
    auc <- sapply(auc, function(x) unlist(slot(x, "y.values")))
    auc <- as.numeric(formatC(auc, digits=4, format="f"))
    out <- new("PerformanceMulti",
                performance = perf,
                auc = auc,
                categories = pred@categories,
                labels = pred@labels)
}

