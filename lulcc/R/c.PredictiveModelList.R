#' @include class-PredictiveModelList.R
NULL

#' Merge PredictiveModelList objects
#'
#' Combine different PredictiveModelList objects into one
#'
#' @param ... two or more PredictiveModelList objects
#' @param recursive for consistency with generic method (ignored)
#'
#' @return a PredictiveModelList object
#'
#' @export
#' @rdname c.PredictiveModelList
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Plum Island Ecosystems
#'
#' ## load data
#' data(pie)
#' 
#' ## observed maps
#' obs <- ObsLulcRasterStack(x=pie,
#'                    pattern="lu", 
#'                    categories=c(1,2,3), 
#'                    labels=c("Forest","Built","Other"), 
#'                    t=c(0,6,14))
#' 
#' ## explanatory variables
#' ef <- ExpVarRasterList(x=pie, pattern="ef")
#' 
#' part <- partition(x=obs[[1]], size=0.1, spatial=TRUE)
#' train.data <- getPredictiveModelInputData(obs=obs, ef=ef, cells=part[["train"]], t=0)
#' 
#' forms <- list(Built ~ ef_001+ef_002+ef_003,
#'               Forest ~ 1,
#'               Other ~ ef_001+ef_002)
#' 
#' glm.models <- glmModels(formula=forms, family=binomial, data=train.data, obs=obs)
#' glm.models
#'
#' ## separate glm.models into two PredictiveModelList objects
#' mod1 <- glm.models[[1]]
#' mod2 <- glm.models[[2:3]]
#'
#' ## put them back together again
#' glm.models <- c(mod1, mod2)
#' glm.models
#'
#' }

#' @rdname c.PredictiveModelList
#' @method c PredictiveModelList
#' @export
c.PredictiveModelList <- function(..., recursive=FALSE) {

    objects <- list(...)
    if (length(objects) == 1) {
        return(objects[[1]])
    }

    if (!all(sapply(objects, FUN=function(x) inherits(x, "PredictiveModelList")))) {
        stop("All objects should inherit from class 'PredictiveModelList'")
    }

    models <- list()
    categories <- list()
    labels <- list()
    for (i in 1:length(objects)) {
        obj        <- objects[[i]]
        models     <- c(models, obj@models)
        categories <- c(categories, obj@categories)
        labels     <- c(labels, obj@labels)
    }

    categories <- unlist(categories)
    labels <- unlist(labels)

    if (any(duplicated(categories))) {
        warning("removing duplicates")
        duplicated.ix <- !duplicated(categories)
        models     <- models[duplicated.ix]
        categories <- categories[duplicated.ix]
        labels     <- categories[duplicated.ix]
    }
        
    new("PredictiveModelList",
        models=models,
        categories=categories,
        labels=labels)

}
    
