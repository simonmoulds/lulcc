#' @include class-ObsLulcMaps.R class-ExpVarMaps.R
NULL

#' Summary
#'
#' Summarize lulcc objects
#'
#' @param object TODO
#' @param ... TODO
#'
#' @return TODO
#' 
#' @export
#' @rdname summary-methods
#'
#' @examples
#'
#' ## TODO
setGeneric("summary")

#' @rdname summary-methods
#' @aliases summary,ObsLulcMaps-method
setMethod("summary", "ObsLulcMaps",
          function(object, ...) {

              sum <- sapply(unstack(object), FUN=function(x) summary(x))
              rownames(sum) <- rownames(summary(object[[1]]))
              colnames(sum) <- names(object)
              sum
              
              ## tot <- as.data.frame(total(object, categories=object@categories)[[1]])
              ## nas <- sapply(unstack(object), FUN=function(x) length(which(is.na(getValues(x)))))
              ## tot <- rbind(tot, nas)
              ## colnames(tot) <- names(object)
              ## rownames(tot) <- c(object@labels, "NA's")
              ## tot
              
          }
          )

#' @rdname summary-methods
#' @aliases summary,ExpVarMaps-method
setMethod("summary", "ExpVarMaps",
          function(object, ...) {
              sum <- sapply(object@maps, FUN=function(x) summary(x[[1]]))
              rownames(sum) <- rownames(summary(object@maps[[1]][[1]]))
              colnames(sum) <- names(object)
              if (object@dynamic) {
                  warning("Only variables corresponding to the initial time step are summarized here")
              }
              
              sum
          }
          )

#' @rdname summary-methods
#' @aliases summary,PredModels-method
setMethod("summary", "PredModels",
          function(object, ...) {

              sums <- list()
              for (i in 1:length(object)) {
                  sums[[i]] <- summary(object@models[[i]])
              }
              names(sums) <- names(object)
              sums
          }
          )

#' @rdname summary-methods
#' @aliases summary,Prediction-method
setMethod("summary", "Prediction",
          function(object, ...) {
              
              sums <- list()
              for (i in 1:length(object)) {
                  sums[[i]] <- summary(object@prediction[[i]])
              }
              names(sums) <- names(object)
              sums
          }
          )

#' @rdname summary-methods
#' @aliases summary,Performance-method
setMethod("summary", "Performance",
          function(object, ...) {
              sum <- as.data.frame(matrix(data=NA, nrow=1, ncol=length(object)))
              sum[1,] <- object@auc
              colnames(sum) <- names(object)
              rownames(sum) <- "AUC"
              sum
          }
          )


## #' @rdname summary-methods
## #' @aliases summary,ModelInput-method
## setMethod("summary", "ModelInput",
##           function(object, ...) {
##           }
##           )
