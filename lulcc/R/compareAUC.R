#' @include class-PredictionList.R
NULL

#' Calculate the area under the ROC curve (AUC)
#'
#' Estimate the AUC for each \code{ROCR::\link[ROCR]{prediction}} object in a
#' \code{\link{PredictionList}} object.
#'
#' The user can compare the performance of different statistical models by
#' providing a list of \code{PredictionList} objects. Note that \code{compareAUC}
#' should be used in conjunction with other comparison methods because the AUC
#' does not contain as much information as, for instance, the ROC curve itself
#' (Pontius and Parmentier, 2014).
#'
#' @param pred a PredictionList object or a list of these
#' @param digits numeric indicating the number of digits to be displayed after
#'   the decimal point for AUC values
#' @param \dots additional arguments (none) 
#'
#' @seealso \code{\link{PredictionList}}, \code{ROCR::\link[ROCR]{performance}}
#' @return A data.frame.
#' @export
#' @rdname compareAUC
#'
#' @references
#' Sing, T., Sander, O., Beerenwinkel, N., Lengauer, T. (2005). ROCR: visualizing
#' classifier performance in R. Bioinformatics 21(20):3940-3941.
#'
#' Pontius Jr, R. G., & Parmentier, B. (2014). Recommendations for using the
#' relative operating characteristic (ROC). Landscape ecology, 29(3), 367-382.
#'
#' @examples
#'
#' ## see PredictiveModelList examples

setGeneric("compareAUC", function(pred, ...)
           standardGeneric("compareAUC"))

#' @rdname compareAUC
#' @aliases compareAUC,PredictionList-method
setMethod("compareAUC", signature(pred = "PredictionList"),
          function(pred, digits=4, ...) {
              auc <- performance(pred@prediction, measure="auc")
              auc <- sapply(auc, function(x) unlist(slot(x, "y.values")))
              out <- as.data.frame(matrix(data=NA, nrow=1, ncol=length(auc)))
              out[1,] <- formatC(auc, digits=digits, format="f")
              colnames(out) <- pred@labels
              out
          }
)

#' @rdname compareAUC
#' @aliases compareAUC,list-method
setMethod("compareAUC", signature(pred = "list"),
          function(pred, digits=4, ...) {

              c1 <- all(sapply(pred, function(x) is(x, "PredictionList")))
              if (!c1) stop("all objects in list should have class PredictionList")

              if (length(pred) == 1) {
                  out <- compareAUC(pred[[1]], ...)
                  
              } else {

                  categories <- unique(unlist(lapply(pred, function(x) x@categories)))
                  labels <- unique(unlist(lapply(pred, function(x) x@labels)))
                  ix <- order(categories)
                  categories <- categories[ix]
                  labels <- labels[ix]

                  out <- as.data.frame(matrix(data=NA, nrow=length(pred), ncol=length(categories)))
                  
                  for (i in 1:length(pred)) {
                      p <- pred[[i]]
                      ix <- which(categories %in% p@categories)
                      auc <- performance(p@prediction, measure="auc")
                      auc <- sapply(auc, function(x) unlist(slot(x, "y.values")))
                      out[i,ix] <- formatC(auc, digits=digits, format="f")      
                  }

                  colnames(out) <- labels
                  rownames(out) <- names(pred)

              }
              
              out

          }
)
