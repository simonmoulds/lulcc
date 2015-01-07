#' Class AgreementBudget
#'
#' An S4 class for information about sources of agreement and disagreement
#' between three maps.
#'
#' @slot agreement data.frame containing sources of agreement and disagreement.
#'   Rows represent different resolutions and columns represent the different
#'   types of agreement and disagreement
#' @slot factors numeric vector of aggregation factors
#' @slot type character. Either 'overall' (considering all possible transitions),
#'   'category' (all transitions from one category) or 'transition' (specific
#'   transition)
#' @slot categories numeric vector of land use categories in the three
#'   maps considered
#' @slot labels character vector with labels corresponding to \code{categories}
#' @slot from,to numeric vectors that together define land use transitions
#'   for which agreement was calculated
#'
#' @author Simon Moulds
#'
#'
#' @export
#' @exportClass AgreementBudget
#' @rdname AgreementBudget-class

setClass("AgreementBudget",
         slots = c(agreement = "data.frame",
                   factors = "numeric",
                   type = "character",         
                   categories = "numeric",
                   labels = "character",
                   from = "numeric",
                   to = "numeric"),
         validity = function(object) {
             ## TODO
             return(TRUE)
         }
)

