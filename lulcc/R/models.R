#' @include class-PredictiveModelList.R class-ObsLulcRasterStack.R class-ExpVarRasterList.R
NULL

#' Fit predictive models 
#'
#' These functions fit parametric and non-parametric models to data.
#'
#' @param formula list containing formula objects
#' @param family see \code{\link[stats]{glm}}. Default is 'binomial'. Only used by
#'   \code{glmModels}
#' @param model see \code{\link[stats]{glm}}. Default is FALSE. Only used by
#'   \code{glmModels}
#' @param ... additional arguments to specific functions
#' @param obs an ObsLulcRasterStack object
#' @param categories numeric vector of land use categories in observed maps.
#'   Only required if 'obs' is missing
#' @param labels character vector (optional) with labels corresponding to
#'   \code{categories}. Only required if 'obs' is missing
#'
#' @seealso \code{\link[stats]{glm}}, \code{rpart::\link[rpart]{rpart}},
#'   \code{randomForest::\link[randomForest]{randomForest}}
#' @return A PredictiveModelList object.
#'
#' @name Model fitting
#' @rdname Model-fitting
#'
#' @examples
#'
#' ## see lulcc-package examples
NULL

#' @export 
#' @rdname Model-fitting
glmModels <- function(formula, family=binomial, model=FALSE, ..., obs, categories=NA, labels=NA) {
    
    glm.models <- list()

    if (!missing(obs)) {
        categories <- obs@categories
        labels <- obs@labels
    }
    formula <- .checkFormula(formula, categories, labels)
    
    for (i in 1:length(formula)) {
        form <- formula[[i]]
        glm.models[[i]] <- glm(form, family=family, model=model, ...)
    }

    out <- new("PredictiveModelList",
               models=glm.models,
               categories=categories,
               labels=labels)
}

#' @export
#' @rdname Model-fitting
randomForestModels <- function(formula, ..., obs, categories=NA, labels=NA) {

    rf.models <- list()

    if (!missing(obs)) {
        categories <- obs@categories
        labels <- obs@labels
    }
    formula <- .checkFormula(formula, categories, labels)
    
    for (i in 1:length(formula)) {
        form <- formula[[i]]
        rf.models[[i]] <- randomForest::randomForest(form, ...)
    }

    out <- new("PredictiveModelList",
               models=rf.models,
               categories=categories,
               labels=labels)
}

#' @export
#' @rdname Model-fitting
rpartModels <- function(formula, ..., obs, categories=NA, labels=NA) {

    rpart.models <- list()

    if (!missing(obs)) {
        categories <- obs@categories
        labels <- obs@labels
    }
    formula <- .checkFormula(formula, categories, labels)
    
    for (i in 1:length(formula)) {
        form <- formula[[i]]
        rpart.models[[i]] <- rpart::rpart(form, method="class", ...)
    }

    out <- new("PredictiveModelList",
               models=rpart.models,
               categories=categories,
               labels=labels)
}


.checkFormula <- function(formula, categories, labels) {
    dep <- sapply(formula, function(x) as.character(x)[2])

    if (is.na(categories) | is.na(labels)) {
        stop("'categories' and 'labels' must be supplied if 'obs' is missing")
    } 

    if (length(categories) != length(labels)) {
        stop("'labels' must correspond to 'categories'")
    }

    ## if (!missing(obs)) {
    ##     categories <- obs@categories
    ##     labels <- obs@labels
    ## } else {
    ##     if (missing(categories) | missing(labels)) {
    ##         stop("'categories' and 'labels' must be supplied if 'obs' is missing")
    ##     } else {
    ##         if (length(categories) != length(labels)) {
    ##             stop("'labels' must correspond to 'categories'")
    ##         }
    ##     }
    ## }

    if (!all(labels %in% dep)) {
        stop("a formula must be supplied for each land use type")
    }

    formula <- formula[match(dep, labels)]
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

##     out <- new("PredictiveModelList",
##                models=glm.models,
##                categories=obs@categories,
##                labels=obs@labels,
##                prediction=list(),
##                performance=list())
## }

