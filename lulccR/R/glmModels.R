#' @include class-PredModels.R class-ObsLulcMaps.R class-ExpVarMaps.R
NULL

#' Model fitting
#'
#' Fit glm models to spatial data
#'
#' @param formula list containing formula objects
#' @param family see \code{\link[stats]{glm}}. Default is 'binomial'
#' @param data see \code{\link[stats]{glm}}
#' @param obs an ObsLulcMaps object
#' @param categories numeric vector of land use categories in observed maps.
#'   Only required if 'obs' is missing
#' @param labels character vector (optional) with labels corresponding to
#'   \code{categories}. Only required if 'obs' is missing
#' @param model see \code{\link[stats]{glm}}. Default is FALSE
#' @param ... additional arguments to \code{\link[stats]{glm}}
#'
#' @seealso \code{\link[stats]{glm}}, \code{\link{PredModels}}
#' @return A PredModels object.
#'
#' @export
#' @rdname glmModels
#'
#' @examples
#'
#' ## see lulccR-package examples

glmModels <- function(formula, family=binomial, data, obs, categories, labels, model=FALSE, ...) {

    glm.models <- list()
    dep <- sapply(formula, function(x) as.character(x)[2])
    if (!missing(obs)) {
        categories <- obs@categories
        labels <- obs@labels
    } else {
        if (missing(categories) | missing(labels)) {
            stop("'categories' and 'labels' must be supplied if 'obs' is missing")
        } else {
            if (length(categories) != length(labels)) {
                stop("'labels' must correspond to 'categories'")
            }
        }
    }

    if (!all(labels %in% dep)) {
        stop("a formula must be supplied for each land use type")
    }
    
    formula <- formula[match(dep, labels)]
    
    for (i in 1:length(formula)) {
        form <- formula[[i]]
        glm.models[[i]] <- glm(form, family=family, data=data, model=model, ...)
    }

    out <- new("PredModels",
               models=glm.models,
               categories=categories,
               labels=labels)
               ## prediction=list(),
               ## performance=list())
}

## glmModels <- function(formula, family=binomial, data, obs, model=FALSE, ...) {

##     glm.models <- list()
##     dep <- sapply(formula, function(x) as.character(x)[2])
##     if (!all(obs@labels %in% dep)) {
##         stop("a formula must be supplied for each land use type in 'obs'")
##     }

##     formula <- formula[match(dep, obs@labels)]
    
##     for (i in 1:length(formula)) {
##         form <- formula[[i]]
##         glm.models[[i]] <- glm(form, family=family, data=data, model=model, ...)
##     }

##     out <- new("PredModels",
##                models=glm.models,
##                categories=obs@categories,
##                labels=obs@labels,
##                prediction=list(),
##                performance=list())
## }

