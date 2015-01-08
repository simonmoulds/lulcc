#' @include class-PredictionMulti.R
NULL

#' Compare the area under the ROC curve (AUC) for different predictive models
#'
#' Estimate the AUC for each \code{ROCR::\link[ROCR]{prediction}} object in a
#' \code{\link{PredictionMulti}} object.
#'
#' The user can compare the performance of different statistical models by
#' providing a list of \code{PredictionMulti} objects. Note that
#' \code{compareAUC} should be used in conjunction with other comparison methods
#' because the AUC does not contain as much information as, for instance, the ROC
#' curve itself
#'
#' @param pred a \code{PredictionMulti} object or a list of these
#' @param digits numeric indicating the number of digits to be displayed after
#'   the decimal point for AUC values
#' @param \dots additional arguments (none) 
#'
#' @seealso \code{\link{PredictionMulti}}, \code{ROCR::\link[ROCR]{performance}}
#' @return data.frame containing AUC values
#' @export
#' @rdname compareAUC
#'
#' @references Sing, T., Sander, O., Beerenwinkel, N., Lengauer, T. (2005).
#' ROCR: visualizing classifier performance in R. Bioinformatics
#' 21(20):3940-3941.

setGeneric("compareAUC", function(pred, ...)
           standardGeneric("compareAUC"))

#' @rdname compareAUC
#' @aliases compareAUC,PredictionMulti-method
setMethod("compareAUC", signature(pred = "PredictionMulti"),
          function(pred, digits=4, ...) {
              auc <- ROCR::performance(pred@prediction, measure="auc")
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

              c1 <- all(sapply(pred, function(x) is(x, "PredictionMulti")))
              if (!c1) stop("all objects in list should have class PredictionMulti")

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
                      auc <- ROCR::performance(p@prediction, measure="auc")
                      auc <- sapply(auc, function(x) unlist(slot(x, "y.values")))
                      out[i,ix] <- formatC(auc, digits=digits, format="f")      
                  }

                  colnames(out) <- labels
                  rownames(out) <- names(pred)

              }
              
              out

          }
)
