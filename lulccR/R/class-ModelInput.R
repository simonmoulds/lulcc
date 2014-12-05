#' @include class-NeighbMaps.R class-Predictors.R class-StatModels.R class-ObservedMaps.R
NULL

setClassUnion("NeighbMapsOrNULL", c("NeighbMaps", "NULL"))
setClassUnion("matrixOrNULL", c("matrix", "NULL"))
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("RasterLayerOrNULL", c("RasterLayer", "NULL"))

#' Class ModelInput
#'
#' An S4 class to represent common inputs to land use change models.
#' 
#' @slot obs RasterLayer showing initial land use 
#' @slot categories numeric vector of land use categories 
#' @slot labels character vector corresponding to \code{categories}
#' @slot pred a PredictorMaps object
#' @slot models a StatModels object
#' @slot time numeric vector containing timesteps over which simulation will
#'   occur
#' @slot demand matrix containing demand scenario or NULL
#' @slot hist RasterLayer showing land use history or NULL
#' @slot mask RasterLayer showing masked areas or NULL
#' @slot neighb NeighbMaps object or NULL
#' @slot rules matrix with land use change decision rules
#' @slot nb.rules numeric with neighbourhood decision rules
#' @slot elas numeric indicating elasticity to change (only required for
#'   \code{CluesModelInput} objects
#' @slot params list with model parameters
#'
#' @author Simon Moulds
#'
#' @rdname class-ModelInput
#'
#' @export
#' @exportClass ModelInput CluesModelInput OrderedModelInput

setClass("ModelInput",
         slots = c(map0 = "RasterLayerOrNULL",           
                   categories = "numeric",
                   labels = "character",
                   pred = "Predictors",
                   models = "StatModels",
                   time = "numeric",
                   demand = "matrixOrNULL",
                   hist = "RasterLayerOrNULL",           
                   mask = "RasterLayerOrNULL",           
                   neighb = "NeighbMapsOrNULL"),         
         validity = function(object) {
             ## TODO
             return(TRUE)
         }
)

#' @rdname class-ModelInput
setClass("CluesModelInput",
         contains = "ModelInput",
         slots = c(rules = "matrixOrNULL",
                   nb.rules = "numericOrNULL",
                   elas = "numeric",
                   params = "list"),
         validity = function(object) {
             ## TODO
             return(TRUE)
         }
)

#' @rdname class-ModelInput
setClass("OrderedModelInput",
         contains = "ModelInput",
         slots = c(rules = "matrixOrNULL",
                   nb.rules = "numericOrNULL",
                   params = "list"),
         validity = function(object) {
             ## TODO
             return(TRUE)
         }
)

