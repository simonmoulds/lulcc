#' @include class-ModelInput.R
#' @include class-NeighbMaps.R
#' @include class-ObsLulcMaps.R
#' @include class-ExpVarMaps.R
#' @include class-PredModels.R
NULL

#' Create a ModelInput object
#'
#' Methods to combine several object classes that are useful for land use change
#' modelling and perform checks to ensure the objects are compatible in time and
#' space for a model simulation.
#' 
#' @param obs an ObsLulcMaps or ModelInput object
#' @param ef an ExpVarMaps object
#' @param time numeric vector containing timesteps over which simulation will
#'   occur  
#' @param demand matrix with demand for each land use category in terms of number
#'   of cells to be allocated. The first row should be the number of cells
#'   allocated to the initial observed land use map (i.e. the land use map for
#'   time 0)
#' @param \dots additional arguments (none)
#'
#' @seealso \code{\link{ModelInput-class}}
#' @return A ModelInput object.
#'
#' @export
#' @rdname ModelInput
#'
#' @examples
#'
#' ## see lulccR-package examples

setGeneric("ModelInput", function(obs, ef, ...)
           standardGeneric("ModelInput"))

#' @rdname ModelInput
#' @aliases ModelInput,ObsLulcMaps,ExpVarMaps-method
setMethod("ModelInput", signature(obs = "ObsLulcMaps", ef = "ExpVarMaps"),
           function(obs, ef, time, demand, ...) {

               ## check that all maps have the same projection
               cr <- lapply(ef@maps, FUN=function(x) compareRaster(obs, x, extent=FALSE, rowcol=FALSE, crs=TRUE, res=FALSE, orig=FALSE, stopiffalse=TRUE))
               
               ## ## check x and models refer to the same categories
               ## if (!all(obs@categories == models@categories)) {
               ##     stop("'models' does not correspond with land use categories in 'obs'")
               ## }

               ## check t starts from 0
               if (time[1] != 0) stop("first timestep in 't' must be 0")

               ## check dimensions of demand and time
               if (ncol(demand) != length(obs@categories)) {
                   stop("number of columns in 'demand' must equal number of land use categories")
               }              
               if (nrow(demand) != length(time)) {
                   stop("number of rows in 'demand' must equal number of timesteps in 't'")
               }

               ## ## check whether hist and mask exist and have correct extent
               ## if (missing(hist)) {
               ##     hist <- raster::getValues(obs[[1]])
               ##     hist[!is.na(hist)] <- 1
               ##     hist <- raster::setValues(obs[[1]], hist)
               ## } else {
               ##     hist <- raster::resample(hist, )
               ##     ##if (!checkExtent(points, hist)) stop("'hist' has NAs in study region")
               ## }

               ## if (missing(mask)) {
               ##     mask <- raster::getValues(obs[[1]])
               ##     mask[!is.na(mask)] <- 1
               ##     mask <- raster::setValues(obs[[1]], mask)
               ## } else {
               ##     mask <- raster::resample(mask, )
               ##     ##if (!checkExtent(points, mask)) stop("'mask' has NAs in study region")
               ## }

               ## ## create neighbourhood maps if required and check dimensions of nb.rules
               ## if (!is.null(neighb)) {
               ##     if (!is(neighb, "NeighbMaps")) stop("'neighb' should be an object of class 'NeighbMaps'")
               ##          neighb <- NeighbMaps(x=obs[[1]], neighb=neighb)                  
               ## }

               ## create ModelInput object
               out <- new("ModelInput",
                          obs=obs,
                          ef=ef,
                          time=time,
                          demand=demand,
                          categories=obs@categories,
                          labels=obs@labels)
           }
)

#' Create a CluesModel object
#'
#' Methods to create a \code{CluesModel} object to supply to
#' \code{\link{allocate}}.
#'
#' The \code{params} argument is a list of parameter values which should contain
#' the following components:
#' 
#' \describe{
#'   \item{\code{jitter.f}}{Parameter controlling the amount of perturbation
#'     applied to the probability surface prior to running the CLUE-S iterative
#'     algorithm. Higher values result in more perturbation. Default is 0.0001}
#'   \item{\code{scale.f}}{Scale factor which controls the amount by which
#'     suitability is increased if demand is not met. Default is 0.0005}
#'   \item{\code{max.iter}}{The maximum number of iterations in the simulation}
#'   \item{\code{max.diff}}{The maximum allowed difference between allocated and
#'     demanded area of any land use type. Default is 5}
#'   \item{\code{ave.diff}}{The average allowed difference between allocated and
#'     demanded area. Default is 5}
#' }
#'
#' Note that, in order to achieve convergence, it is likely that some adjustment
#' of these parameters will be required.
#'
#' @param x a ModelInput object
#' @param models a PredModels object
#' @param hist RasterLayer containing land use history (values represent the
#'   number of years the cell has contained the current land use category)
#' @param mask RasterLayer containing binary values where 0 indicates cells
#'   that are not allowed to change
#' @param neighb an object of class NeighbMaps
#' @param rules matrix with land use change decision rules
#' @param nb.rules numeric with neighbourhood decision rules
#' @param elas numeric indicating the elasticity of each land use category to
#'   change. Elasticity varies between 0 and 1, with 0 indicating a low
#'   resistance to change and 1 indicating a high resistance to change
#' @param params list with model parameters
#' @param output either a RasterStack containing output maps or NULL
#' @param \dots additional arguments (none)
#'
#' @seealso \code{\link{CluesModel-class}}, \code{\link{allocate}}
#'
#' @return A CluesModel object.
#'
#' @export
#' @rdname CluesModel
#'
#' @references
#' Verburg, P.H., Soepboer, W., Veldkamp, A., Limpiada, R., Espaldon, V., Mastura,
#' S.S. (2002). Modeling the spatial dynamics of regional land use: the CLUE-S
#' model. Environmental management, 30(3):391-405.
#'
#' @examples
#'
#' ## see lulccR-package examples

setGeneric("CluesModel", function(x, ...)
           standardGeneric("CluesModel"))

#' @rdname CluesModel
#' @aliases CluesModel,ModelInput-method
setMethod("CluesModel", signature(x = "ModelInput"),
          function(x, models, hist, mask, neighb=NULL, elas, rules=NULL, nb.rules=NULL, params, output=NULL, ...) {

              ## check x and models refer to the same categories
              if (!all(x@obs@categories == models@categories)) {
                  stop("'models' does not correspond with land use categories in 'obs'")
              }

              ## check whether hist and mask exist and have correct extent
              if (missing(hist)) {
                  hist <- x@obs[[1]]
                  hist[!is.na(hist)] <- 1
              } ## else {
              ##     hist <- raster::resample(hist, )
              ##     ##if (!checkExtent(points, hist)) stop("'hist' has NAs in study region")
              ## }

              if (missing(mask)) {
                  mask <- x@obs[[1]]
                  mask[!is.na(mask)] <- 1
              } ## else {
              ##     mask <- raster::resample(mask, )
              ##     ##if (!checkExtent(points, mask)) stop("'mask' has NAs in study region")
              ## }

              ## create neighbourhood maps if required and check dimensions of nb.rules
              if (!is.null(neighb)) {
                  if (!is(neighb, "NeighbMaps")) stop("'neighb' should be an object of class 'NeighbMaps'")
                  ## recalculate neighbourhood for initial observed map
                  neighb <- NeighbMaps(x=x@obs[[1]], neighb=neighb)                  
              }
              
              if (!is.null(rules)) {
                  if (!all(dim(rules) %in% length(x@categories))) {
                      stop("'rules' must be square matrix with dimensions equal to number of land use categories")
                  }
              } 

              if (!is.null(neighb) && !is.null(nb.rules)) {
                  if (length(nb.rules) != length(neighb)) {
                      stop("rule should be provided for each neighbourhood map")
                  }
                  
              } else if (is.null(neighb) && !is.null(nb.rules)) {
                  warning("neighb is NULL: neighbourhood decision rules not implemented")
                  nb.rules <- NULL
              }

              if (length(elas) != length(x@obs@categories)) {
                  stop("'elas' must be numeric vector with length equal to number of land use categories")
              }

              if (missing(params)) {
                  params <- .checkCluesParams()
              } else {
                  params <- .checkCluesParams(params)
              }
                  
              out <- new("CluesModel", x, models=models, hist=hist, mask=mask, neighb=neighb, elas=elas, rules=rules, nb.rules=nb.rules, params=params, output=output)
             
          }
)

.checkCluesParams <- function(params) {
    if (missing(params) || length(params) == 0) {
        params <- list(jitter.f=0.0001, scale.f=0.0005, max.iter=1000, max.diff=5, ave.diff=5)
    } else {
        if (is.null(names(params)) || any(nchar(names(params)) == 0)) stop("'params' must be a named list") 
        if (!"jitter.f" %in% names(params)) params <- c(params, list(jitter.f=0.0001))
        if (!"scale.f" %in% names(params))  params <- c(params, list(scale.f=0.0005))
        if (!"max.iter" %in% names(params)) params <- c(params, list(max.iter=1000))
        if (!"max.diff" %in% names(params)) params <- c(params, list(max.diff=5))
        if (!"ave.diff" %in% names(params)) params <- c(params, list(ave.diff=5))
        ## TODO check reasonable values
    }
    params <- params[c("jitter.f","scale.f","max.iter","max.diff","ave.diff")]
}

#' Create an OrderedModel object
#'
#' Methods to create a \code{OrderedModel} object to supply to
#' \code{\link{allocate}}.
#'
#' The \code{params} argument is a list of parameter values which should contain
#' the following components:
#' 
#' \describe{
#'   \item{\code{max.diff}}{The maximum allowed difference between allocated and
#'     demanded area of any land use type. Default is 5}
#' }
#' 
#' @param x a ModelInput object
#' @param models a PredModels object
#' @param hist RasterLayer containing land use history (values represent the
#'   number of years the cell has contained the current land use category)
#' @param mask RasterLayer containing binary values where 0 indicates cells
#'   that are not allowed to change
#' @param neighb an object of class NeighbMaps
#' @param rules matrix with land use change decision rules
#' @param nb.rules numeric with neighbourhood decision rules
#' @param order numeric vector of land use categories in the order that change
#'   should be allocated. See Details
#' @param params list with model parameters
#' @param output either a RasterStack containing output maps or NULL
#' @param \dots additional arguments (none)
#'
#' @seealso \code{\link{OrderedModel-class}}, \code{\link{allocate}}
#'
#' @return An OrderedModel object.
#'
#' @export
#' @rdname OrderedModel
#'
#' @references
#' Fuchs, R., Herold, M., Verburg, P.H., and Clevers, J.G.P.W. (2013). A
#' high-resolution and harmonized model approach for reconstructing and analysing
#' historic land changes in Europe, Biogeosciences, 10:1543-1559.
#'
#' @examples
#'
#' ## see lulccR-package examples
 
setGeneric("OrderedModel", function(x, ...)
           standardGeneric("OrderedModel"))

#' @rdname OrderedModel
#' @aliases OrderedModel,ModelInput-method
setMethod("OrderedModel", signature(x = "ModelInput"),
          function(x, models, hist, mask, neighb=NULL, rules=NULL, nb.rules=NULL, order, params, output=NULL, ...) {

              ## check x and models refer to the same categories
              if (!all(x@obs@categories == models@categories)) {
                  stop("'models' does not correspond with land use categories in 'obs'")
              }

              ## check whether hist and mask exist and have correct extent
              if (missing(hist)) {
                  hist <- x@obs[[1]]
                  hist[!is.na(hist)] <- 1
              } ## else {
              ##     hist <- raster::resample(hist, )
              ##     ##if (!checkExtent(points, hist)) stop("'hist' has NAs in study region")
              ## }

              if (missing(mask)) {
                  mask <- x@obs[[1]]
                  mask[!is.na(mask)] <- 1
              } ## else {
              ##     mask <- raster::resample(mask, )
              ##     ##if (!checkExtent(points, mask)) stop("'mask' has NAs in study region")
              ## }

              ## create neighbourhood maps if required and check dimensions of nb.rules
              if (!is.null(neighb)) {
                  if (!is(neighb, "NeighbMaps")) stop("'neighb' should be an object of class 'NeighbMaps'")
                  ## recalculate neighbourhood for initial observed map
                  neighb <- NeighbMaps(x=x@obs[[1]], neighb=neighb)                  
              }

              if (!is.null(rules)) {
                  if (!all(dim(rules) %in% length(x@obs@categories))) {
                      stop("'rules' must be square matrix with dimensions equal to number of land use categories")
                  }
              } 

              if (!is.null(neighb) && !is.null(nb.rules)) {
                  if (length(nb.rules) != length(neighb)) {
                      stop("rule should be provided for each neighbourhood map")
                  }
                  
              } else if (is.null(neighb) && !is.null(nb.rules)) {
                  warning("neighb is NULL: neighbourhood decision rules not implemented")
                  nb.rules <- NULL
              }

              if (missing(order)) {
                  stop("missing argument 'order'")
              } else {
                  if (!all(order %in% x@categories)) {
                      stop("argument 'order' should contain exactly the same categories as x@categories (but not necessarily in the same order)")
                  }
              }
              
              if (missing(params)) {
                  params <- .checkOrderedParams()
              } else {
                  params <- .checkOrderedParams(params)
              }
              
              out <- new("OrderedModel", x, models=models, hist=hist, mask=mask, neighb=neighb, rules=rules, nb.rules=nb.rules, order=order, params=params, output=output)
             
          }
)

.checkOrderedParams <- function(params) {
    params <- list()
}

##     if (missing(params) || length(params) == 0) {
##         params <- list(max.diff=5)
##     } else {
##         if (is.null(names(params)) || any(nchar(names(params)) == 0)) stop("'params' must be a named list") 
##         if (!all(c("max.diff") %in% params)) {
##             stop("'params' does not contain the required parameter set")
##         }
##         ## TODO check reasonable values
##     }
##     params <- params[c("max.diff")]
## }
