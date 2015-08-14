#' @include class-ObsLulcRasterStack.R class-ExpVarRasterList.R
NULL

#' Summary
#'
#' Summarise lulcc objects containing Raster* data or predictive models
#'
#' @param object an object belonging to one of the classes in \code{lulcc}
#' @param ... additional arguments (none)
#'
#' @return A matrix, data.frame or list
#' 
#' @export
#' @rdname summary-methods
#'

setGeneric("summary")

#' @rdname summary-methods
#' @aliases summary,ObsLulcRasterStack-method
setMethod("summary", "ObsLulcRasterStack",
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
#' @aliases summary,ExpVarRasterList-method
setMethod("summary", "ExpVarRasterList",
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
#' @aliases summary,NeighbRasterStack-method
setMethod("summary", "NeighbRasterStack",
          function(object, ...) {

              sum <- sapply(unstack(object), FUN=function(x) summary(x))
              rownames(sum) <- rownames(summary(object[[1]]))
              colnames(sum) <- names(object)
              sum
              
          }
          )

#' @rdname summary-methods
#' @aliases summary,PredictiveModelList-method
setMethod("summary", "PredictiveModelList",
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
#' @aliases summary,Model-method
setMethod("summary", "Model",
          function(object, ...) {

              sum.obs <- summary(object@obs)
              sum.exp <- summary(object@ef)
              sum.input <- cbind(sum.obs, sum.exp)

              if (is(object@output, "RasterStack")) {
                  output.total <- total(object@output, categories=object@categories)$total
                  sum.output <- list()
                  for (i in 1:length(object@categories)) {
                      df <- as.data.frame(matrix(data=NA, nrow=nrow(object@demand), ncol=3))
                      df[,1] <- object@demand[,i]
                      df[,2] <- output.total[,i]
                      df[,3] <- df[,1] - df[,2]
                      label <- object@labels[i]
                      names(df) <- c(paste0(label, "_demand"),
                                     paste0(label, "_alloc"),
                                     paste0(label, "_diff"))
                      sum.output[[i]] <- df
                  }
                  sum.output <- do.call(cbind, sum.output)
              } else {
                  sum.output <- NULL
              }

              sum <- list(input=sum.input, output=sum.output)
              sum
          }
          )


