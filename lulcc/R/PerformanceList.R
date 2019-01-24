#' @include class-PerformanceList.R
NULL

#' Create a PerformanceList object
#'
#' This function uses different measures to evaluate multiple
#' \code{ROCR::\link[ROCR]{prediction}} objects stored in a
#' \code{\link{PredictionList}} object.
#'
#' @param pred an object of class PredictionList
#' @param measure performance measure to use for the evaluation. See
#'   \code{ROCR::\link[ROCR]{performance}}
#' @param x.measure a second performance measure. See
#'   \code{ROCR::\link[ROCR]{performance}}
#' @param \dots additional arguments to \code{ROCR::\link[ROCR]{performance}}
#' 
#' @seealso \code{\link{performance}}, \code{\link{PredictionList}}
#' @return A \code{PerformanceList} object.
#'
#' @export
#'
#' @references Sing, T., Sander, O., Beerenwinkel, N., Lengauer, T. (2005).
#' ROCR: visualizing classifier performance in R. Bioinformatics
#' 21(20):3940-3941.
#'
#' @examples
#'
#' ## see lulcc-package examples

PerformanceList <- function(pred, measure, x.measure="cutoff", ...) {
    perf <- performance(pred@prediction, measure, x.measure, ...)
    auc <- performance(pred@prediction, measure="auc")
    auc <- sapply(auc, function(x) unlist(slot(x, "y.values")))
    auc <- as.numeric(formatC(auc, digits=4, format="f"))
    out <- new("PerformanceList",
                performance = perf,
                auc = auc,
                categories = pred@categories,
                labels = pred@labels)
}

