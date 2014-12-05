#' Class Predictors
#'
#' An S4 class for predictor variables.
#'
#' @slot maps list of RasterStacks. The length of the list corresponds to the
#'   number of predictor variables and the number of layers in each RasterStack
#'   represents time 
#' @slot calls list of PredictorCall objects
#' @slot map.names character vector of the name of each variable in \code{maps}
#' @slot call.names character vector of the name of each variable in \code{calls} 
#' @slot dynamic logical indicating whether dynamic variables are present
#'
#' @author Simon Moulds
#'
#' @rdname class-Predictors
#'
#' @export
#' @exportClass Predictors
        
setClass("Predictors",
         slots = c(maps = "list",
                   calls = "list",
                   map.names = "character",
                   call.names = "character",
                   dynamic = "logical"),
         validity = function(object) {
             ##check1 <- (length(object@maps) > 0)
             ##if (!check1) stop("empty list")
             ##check2 <- (length(object@maps) == length(object@names))
             ##if (!check2) stop("maps and names have different lengths")
             return(TRUE)           
         }
)
