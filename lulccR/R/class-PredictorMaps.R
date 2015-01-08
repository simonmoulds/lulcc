#' Class PredictorMaps
#'
#' An S4 class for predictor variables.
#'
#' @slot maps list of RasterStacks. The length of the list corresponds to the
#'   number of predictor variables and the number of layers in each RasterStack
#'   represents time 
#' @slot map.names character vector of the name of each variable in \code{maps}
#' @slot dynamic logical indicating whether dynamic variables are present
#'
#' @export
#' @exportClass PredictorMaps
#' @rdname PredictorMaps-class
        
setClass("PredictorMaps",
         slots = c(maps = "list",
                   map.names = "character",
                   dynamic = "logical"),
         validity = function(object) {
             ##check1 <- (length(object@maps) > 0)
             ##if (!check1) stop("empty list")
             ##check2 <- (length(object@maps) == length(object@names))
             ##if (!check2) stop("maps and names have different lengths")
             return(TRUE)           
         }
)
