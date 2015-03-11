#' @include class-Prediction.R
NULL

#' Create a Prediction object
#'
#' This function creates a \code{ROCR::\link[ROCR]{prediction}} object for each
#' predictive model in a \code{PredModels} object. It should be used with
#' \code{\link{Performance}} to evaluate multiple models with exactly the
#' same criteria while keeping track of which model corresponds to which land use
#' category. 
#'
#' @param models a PredModels object
#' @param obs an ObsLulcMaps object
#' @param ef an ExpVarMaps object
#' @param timestep numeric indicating the timestep of the observed map in
#'   \code{obs} against which the observed map should be tested
#' @param partition index of cells for which occurrence should be predicted
#' @param \dots additional arguments to \code{ROCR::\link[ROCR]{prediction}}
#'
#' @seealso \code{link{Performance}}, \code{ROCR::\link[ROCR]{prediction}} 
#' @return A \code{Prediction} object.
#'
#' @export
#'
#' @references Sing, T., Sander, O., Beerenwinkel, N., Lengauer, T. (2005).
#' ROCR: visualizing classifier performance in R. Bioinformatics
#' 21(20):3940-3941.
#'
#' @examples
#'
#' ## see PredModels examples

Prediction <- function(models, obs, ef, timestep=0, partition, ...) {
    ix <- which(obs@t %in% timestep)
    if (length(ix) == 0) stop(paste0("no observed map exists for timestep ", timestep))
    if (missing(partition)) {
        partition <- which(!is.na(raster::getValues(obs)))
    }
    obs.vals <- obs@maps[[ix]][partition]
    newdata <- as.data.frame(ef, cells=partition)
    mod <- calcProb(object=models, newdata=newdata) 
    prediction.list <- list()
    for (j in 1:length(models@models)) {
        labels <- as.numeric(obs.vals == models@categories[j])
        prediction.list[[j]] <- ROCR::prediction(mod[,j], labels, ...)
    }              
    ## out <- new("Prediction", prediction=prediction.list, types=models@types, categories=models@categories, labels=models@labels)
    out <- new("Prediction", prediction=prediction.list, categories=models@categories, labels=models@labels)
}      

