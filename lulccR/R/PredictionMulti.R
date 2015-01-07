#' @include class-PredictionMulti.R
NULL

#' Create a PredictionMulti object
#'
#' This function creates a \code{ROCR::\link[ROCR]{prediction}} object for each
#' statistical model in a \code{StatModels} object. It should be used with
#' \code{\link{PerformanceMulti}} to evaluate multiple models with exactly the
#' same criteria while keeping track of which model corresponds to which land use
#' category. This makes it easier to write functions that compare different model
#' types for the same land use category, such as \code{\link{PerformanceMulti.plot}}.
#'
#' @param models a \code{StatModels} object
#' @param obs an \code{ObservedMaps} object
#' @param pred a \code{Predictors} object
#' @param timestep numeric indicating the timestep of the observed map in
#'   \code{obs} against which the observed map should be tested
#' @param partition character. Either 'train', 'test' or 'none', indicating
#'   whether to use the training or testing partition or the full dataset for
#'   model evaluation. Default is 'test'
#' @param ... additional arguments to \code{ROCR::\link[ROCR]{prediction}}
#'
#' @seealso \code{ROCR::\link[ROCR]{prediction}} 
#' @author Simon Moulds
#' @return a \code{PredictionMulti} object
#'
#' @export
#'
#' @references Sing, T., Sander, O., Beerenwinkel, N., Lengauer, T. (2005).
#' ROCR: visualizing classifier performance in R. Bioinformatics
#' 21(20):3940-3941.

PredictionMulti <- function(models, obs, pred, timestep=0, partition, ...) {
    ix <- which(obs@t %in% timestep)
    if (length(ix) == 0) stop(paste0("no observed map exists for timestep ", timestep))
    if (missing(partition)) {
        partition <- which(!is.na(raster::getValues(obs)))
    }
    obs.vals <- obs@maps[[ix]][partition]
    newdata <- as.data.frame(pred, cells=partition)
    mod <- calcProb(object=models, newdata=newdata) 
    prediction.list <- list()
    for (j in 1:length(models@models)) {
        labels <- as.numeric(obs.vals == models@categories[j])
        prediction.list[[j]] <- ROCR::prediction(mod[,j], labels, ...)
    }              
    out <- new("PredictionMulti", prediction=prediction.list, categories=models@categories, labels=models@labels)
}      

