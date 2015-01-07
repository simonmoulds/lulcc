#' Class ThreeMapComparison
#'
#' An S4 class to hold results of a comparison between a reference map for time
#' 1, a reference map for time 2 and a simulation map for time 2 using the
#' the method described by Pontius et al. (2011).
#'
#' @slot tables list of data.frames that depict the three dimensional table
#'   described by Pontius et al. (2011) at different resolutions
#' @slot factors numeric vector of aggregation factors
#' @slot categories numeric vector of land use categories
#' @slot labels character vector corresponding to \code{categories}
#'
#' @author Simon Moulds
#'
#' @export
#' @exportClass ThreeMapComparison
#' @rdname ThreeMapComparison-class

setClass("ThreeMapComparison",
         representation(
             tables = "list",
             factors = "numeric",
             categories = "numeric",
             labels = "character"),
         validity = function(object) {
             ## TODO
             return(TRUE)
         }
)
