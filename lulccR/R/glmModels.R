#' @include class-PredModels.R class-ObsLulcMaps.R class-ExpVarMaps.R
NULL

#' Model fitting
#'
#' Fit glm models to spatial data
#'
#' @param formula list containing formula objects
#' @param family TODO 
#' @param data TODO
#' @param obs TODO
#' @param model TODO
#' @param ... additional arguments to glm
#'
#' @return TODO
#'
#' @export
#' @rdname glmModels
#'
#' @examples
#'
#' ## TODO

glmModels <- function(formula, family=binomial, data, obs, model=FALSE, ...) {

    glm.models <- list()
    dep <- sapply(formula, function(x) as.character(x)[2])
    if (!all(obs@labels %in% dep)) {
        stop("a formula must be supplied for each land use type in 'obs'")
    }

    formula <- formula[match(dep, obs@labels)]
    
    for (i in 1:length(formula)) {
        form <- formula[[i]]
        glm.models[[i]] <- glm(form, family=family, data=data, model=model, ...)
    }

    out <- new("PredModels",
               models=glm.models,
               categories=obs@categories,
               labels=obs@labels,
               prediction=list(),
               performance=list())
}

