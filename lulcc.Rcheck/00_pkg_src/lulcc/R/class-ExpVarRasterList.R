#' Class ExpVarRasterList
#'
#' An S4 class for explanatory variables.
#'
#' @slot maps list of RasterStack objects. The length of the list corresponds to
#'   the number of explanatory variables and the number of layers in each
#'   RasterStack represents time
#' @slot names character vector with the name of each variable in \code{maps}
#' @slot dynamic logical indicating whether dynamic variables are present
#'
#' @export
#' @exportClass ExpVarRasterList
#' @rdname ExpVarRasterList-class

setClass("ExpVarRasterList",
         slots = c(maps = "list",
                   names = "character",
                   dynamic = "logical"),
         validity = function(object) {
             ##check1 <- (length(object@maps) > 0)
             ##if (!check1) stop("empty list")
             ##check2 <- (length(object@maps) == length(object@names))
             ##if (!check2) stop("maps and names have different lengths")
             return(TRUE)           
         }
)

## setClass("ExpVarRasterList",
##          representation(
##              maps = "list",
##              names = "character",
##              dynamic = "logical"),
##          validity = function(object) {
##              ##check1 <- (length(object@maps) > 0)
##              ##if (!check1) stop("empty list")
##              ##check2 <- (length(object@maps) == length(object@names))
##              ##if (!check2) stop("maps and names have different lengths")
##              return(TRUE)           
##          }
## )
