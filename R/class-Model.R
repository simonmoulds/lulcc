setClassUnion("NeighbRasterStackOrNULL", c("NeighbRasterStack", "NULL"))
setClassUnion("matrixOrNULL", c("matrix", "NULL"))
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("RasterLayerOrNULL", c("RasterLayer", "NULL"))
setClassUnion("RasterStackOrNULL", c("RasterStack", "NULL"))

#' @include class-NeighbRasterStack.R class-ExpVarRasterList.R class-PredictiveModelList.R class-ObsLulcRasterStack.R class-CategoryLabel.R
NULL

#' Virtual class Model
#'
#' A virtual S4 class to represent land use change models.
#'
#' @slot output RasterStack containing simulated land use maps or NULL
#'
#' @export
#' @exportClass Model
#' @rdname Model-class
setClass("Model",
         contains = c("VIRTUAL"),
         slots = c(output = "RasterStackOrNULL"),
         validity = function(object) {
             ## TODO
             return(TRUE)
         }
)

#' Class CluesModel
#'
#' An S4 class to represent inputs to the CLUE-S land use change model.
#'
#' @slot obs an ObsLulcRasterStack object 
#' @slot ef an ExpVarRasterList object
#' @slot models a PredictiveModelList object
#' @slot time numeric vector of timesteps over which simulation will occur
#' @slot demand matrix containing demand scenario
#' @slot hist RasterLayer showing land use history or NULL
#' @slot mask RasterLayer showing masked areas or NULL
#' @slot neighb NeighbRasterStack object or NULL
#' @slot categories numeric vector of land use categories 
#' @slot labels character vector corresponding to \code{categories}
#' @slot rules matrix with land use change decision rules
#' @slot nb.rules numeric with neighbourhood decision rules
#' @slot elas numeric indicating elasticity to change (only required for
#' @slot params list with model parameters
#' @slot output RasterStack containing simulated land use maps or NULL
#'
#' @export
#' @exportClass CluesModel
#' @rdname CluesModel-class

setClass("CluesModel",
         contains = c("Model",
                      "CategoryLabel"),
         slots = c(obs = "ObsLulcRasterStack",           
                   ef = "ExpVarRasterList",
                   models = "PredictiveModelList",
                   time = "numeric",
                   demand = "matrix",
                   hist = "RasterLayerOrNULL",           
                   mask = "RasterLayerOrNULL",           
                   neighb = "NeighbRasterStackOrNULL",         
                   rules = "matrixOrNULL",
                   nb.rules = "numericOrNULL",
                   elas = "numeric",
                   params = "list"),
         validity = function(object) {
             ## TODO
             return(TRUE)
         }
)

#' Class OrderedModel
#'
#' An S4 class to represent inputs to the Ordered allocation procedure
#' 
#' @slot obs an ObsLulcRasterStack object 
#' @slot ef an ExpVarRasterList object
#' @slot models a PredictiveModelList object
#' @slot time numeric vector of timesteps over which simulation will occur
#' @slot demand matrix containing demand scenario
#' @slot hist RasterLayer showing land use history or NULL
#' @slot mask RasterLayer showing masked areas or NULL
#' @slot neighb NeighbRasterStack object or NULL
#' @slot categories numeric vector of land use categories 
#' @slot labels character vector corresponding to \code{categories}
#' @slot rules matrix with land use change decision rules
#' @slot nb.rules numeric with neighbourhood decision rules
#' @slot order numeric vector of land use categories in the order that change
#'   should be allocated
#' @slot params list with model parameters
#' @slot output RasterStack containing simulated land use maps or NULL
#'
#' @export
#' @exportClass OrderedModel
#' @rdname OrderedModel-class
setClass("OrderedModel",
         contains = c("Model",
                      "CategoryLabel"),
         slots = c(obs = "ObsLulcRasterStack",           
                   ef = "ExpVarRasterList",
                   models = "PredictiveModelList",
                   time = "numeric",
                   demand = "matrix",
                   hist = "RasterLayerOrNULL",           
                   mask = "RasterLayerOrNULL",           
                   neighb = "NeighbRasterStackOrNULL",         
                   rules = "matrixOrNULL",
                   nb.rules = "numericOrNULL",
                   order = "numeric",
                   params = "list"),
         validity = function(object) {
             ## TODO
             ## check order only contains values in categories
             return(TRUE)
         }
)         
