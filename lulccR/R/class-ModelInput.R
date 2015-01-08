#' @include class-NeighbMaps.R class-PredictorMaps.R class-StatModels.R class-ObservedMaps.R
NULL

setClassUnion("NeighbMapsOrNULL", c("NeighbMaps", "NULL"))
setClassUnion("matrixOrNULL", c("matrix", "NULL"))
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("RasterLayerOrNULL", c("RasterLayer", "NULL"))
setClassUnion("RasterStackOrNULL", c("RasterStack", "NULL"))

#' Class ModelInput
#'
#' An S4 class to represent common inputs to land use change models.
#'
#' TODO
#' 
#' @slot obs an ObservedMaps object 
#' @slot pred a PredictorMaps object
#' @slot models a StatModels object
#' @slot time numeric vector containing timesteps over which simulation will
#'   occur
#' @slot demand matrix containing demand scenario or NULL
#' @slot hist RasterLayer showing land use history or NULL
#' @slot mask RasterLayer showing masked areas or NULL
#' @slot neighb NeighbMaps object or NULL
#' @slot categories numeric vector of land use categories 
#' @slot labels character vector corresponding to \code{categories}
#'
#' @export
#' @exportClass ModelInput
#' @rdname ModelInput-class
setClass("ModelInput",
         slots = c(obs = "ObservedMaps",           
                   pred = "PredictorMaps",
                   models = "StatModels",
                   time = "numeric",
                   demand = "matrixOrNULL",
                   hist = "RasterLayerOrNULL",           
                   mask = "RasterLayerOrNULL",           
                   neighb = "NeighbMapsOrNULL",         
                   categories = "numeric",
                   labels = "character"),
         validity = function(object) {
             ## TODO
             return(TRUE)
         }
)

#' Virtual class Model
#'
#' An S4 virtual class to represent land use change models.
#'
#' @slot obs an ObservedMaps object 
#' @slot pred a PredictorMaps object
#' @slot models a StatModels object
#' @slot time numeric vector containing timesteps over which simulation will
#'   occur
#' @slot demand matrix containing demand scenario or NULL
#' @slot hist RasterLayer showing land use history or NULL
#' @slot mask RasterLayer showing masked areas or NULL
#' @slot neighb NeighbMaps object or NULL
#' @slot categories numeric vector of land use categories 
#' @slot labels character vector corresponding to \code{categories}
#' @slot output RasterStack containing simulated land use maps or NULL
#'
#' @export
#' @exportClass Model
#' @rdname Model-class
setClass("Model",
         contains = c("ModelInput",
                      "VIRTUAL"),
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
#' TODO
#'
#' @slot obs an ObservedMaps object 
#' @slot pred a PredictorMaps object
#' @slot models a StatModels object
#' @slot time numeric vector containing timesteps over which simulation will
#'   occur
#' @slot demand matrix containing demand scenario or NULL
#' @slot hist RasterLayer showing land use history or NULL
#' @slot mask RasterLayer showing masked areas or NULL
#' @slot neighb NeighbMaps object or NULL
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
         contains = c("ModelInput",
                      "Model"),
         slots = c(rules = "matrixOrNULL",
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
#' An S4 class to represent inputs to the ordered stochastic land use change
#' model.
#'
#' TODO
#' 
#' @slot obs an ObservedMaps object 
#' @slot pred a PredictorMaps object
#' @slot models a StatModels object
#' @slot time numeric vector containing timesteps over which simulation will
#'   occur
#' @slot demand matrix containing demand scenario or NULL
#' @slot hist RasterLayer showing land use history or NULL
#' @slot mask RasterLayer showing masked areas or NULL
#' @slot neighb NeighbMaps object or NULL
#' @slot categories numeric vector of land use categories 
#' @slot labels character vector corresponding to \code{categories}
#' @slot rules matrix with land use change decision rules
#' @slot nb.rules numeric with neighbourhood decision rules
#' @slot params list with model parameters
#' @slot output RasterStack containing simulated land use maps or NULL
#'
#' @export
#' @exportClass OrderedModel
#' @rdname OrderedModel-class
setClass("OrderedModel",
         contains = c("ModelInput",
                      "Model"),
         slots = c(rules = "matrixOrNULL",
                   nb.rules = "numericOrNULL",
                   params = "list"),
         validity = function(object) {
             ## TODO
             return(TRUE)
         }
)

