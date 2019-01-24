#' @include class-PredictionList.R
NULL

#' Create a PredictionList object
#'
#' This function creates a \code{ROCR::\link[ROCR]{prediction}} object for each
#' predictive model in a \code{PredictiveModelList} object. It should be used with
#' \code{\link{PerformanceList}} to evaluate multiple models with exactly the
#' same criteria while keeping track of which model corresponds to which land use
#' category. 
#'
#' @param models a PredictiveModelList object
#' @param newdata a data.frame containing new data
#' @param \dots additional arguments to \code{ROCR::\link[ROCR]{prediction}}
#'
#' @seealso \code{link{PerformanceList}}, \code{ROCR::\link[ROCR]{prediction}} 
#' @return A \code{PredictionList} object.
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

PredictionList <- function(models, newdata, ...) {
    ## ix <- which(obs@t %in% timestep)
    ## if (length(ix) == 0) stop(paste0("no observed map exists for timestep ", timestep))
    ## if (missing(partition)) {
    ##     warning("no partition provided: using entire dataset")
    ##     partition <- which(!is.na(raster::getValues(obs)))  ## i.e all values
    ## }
    ## obs.vals <- obs[[ix]][partition]
    ## newdata <- as.data.frame(ef, cells=partition)
    mod <- predict(object=models, newdata=newdata) 
    prediction.list <- list()
    for (j in 1:length(models@models)) {
        ## labels <- as.numeric(obs.vals == models@categories[j])
        col <- names(models)[j]
        labels <- newdata[,col]
        prediction.list[[j]] <- ROCR::prediction(mod[,j], labels, ...)
    }              
    ## out <- new("PredictionList", prediction=prediction.list, types=models@types, categories=models@categories, labels=models@labels)
    out <- new("PredictionList", prediction=prediction.list, categories=models@categories, labels=models@labels)
}      

