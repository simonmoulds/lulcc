#' Class PredictorCall
#'
#' An S4 class for predictor variables derived from the observed pattern of land
#' use that should be updated during a simulation when a new land use map is
#' available.
#'
#' @slot map RasterLayer showing the predictor variable
#' @slot call function call used to calculate predictor variable
#' @slot update.arg the argument (character) in \code{call} that should be
#'   updated when a new land use map is available
#' @slot name the variable name (character)
#'
#' @author Simon Moulds
#'
#' @rdname class-PredictorCall
#'
#' @export
#' @exportClass PredictorCall

setClass("PredictorCall",
         slots = c(map = "RasterLayer",
                   call = "call",
                   update.arg = "character",
                   name = "character"),
         validity = function(object) {
             ## check args list is named
             c1 <- object@update.arg %in% names(object@call)
             if (!c1) stop("update.arg is not present in call")
             ## TODO
         }
)

                   
                    
