#' @include class-CategoryLabel.R class-Model.R
NULL

#' Class ThreeMapComparison
#'
#' An S4 class to hold results of a comparison between a reference map for time
#' 1, a reference map for time 2 and a simulation map for time 2 using the
#' the method described by Pontius et al. (2011).
#'
#' @slot tables list of data.frames that depict the three dimensional table
#'   described by Pontius et al. (2011) at different resolutions
#' @slot factors numeric vector of aggregation factors
#' @slot maps list of RasterStack objects containing land use maps at different
#'   resolutions
#' @slot categories numeric vector of land use categories
#' @slot labels character vector corresponding to \code{categories}
#'
#' @export
#' @exportClass ThreeMapComparison
#' @rdname ThreeMapComparison-class
#'
#' @references Pontius Jr, R.G., Peethambaram, S., Castella, J.C. (2011).
#' Comparison of three maps at multiple resol utions: a case study of land change
#' simulation in Cho Don District, Vietnam. Annals of the Association of American
#' Geographers 101(1): 45-62.

setClass("ThreeMapComparison",
         contains = c("CategoryLabel"),
         slots = c(maps = "list",
                   tables = "list",
                   factors = "numeric"),
         validity = function(object) {
             ## TODO
             return(TRUE)
         }
)

#' Class AgreementBudget
#'
#' An S4 class for information about sources of agreement and disagreement
#' between three categorical raster maps.
#'
#' @slot tables list of data.frames that depict the three dimensional table
#'   described by Pontius et al. (2011) at different resolutions
#' @slot factors numeric vector of aggregation factors
#' @slot maps list of RasterStack objects containing land use maps at different
#'   resolutions
#' @slot categories numeric vector of land use categories
#' @slot labels character vector corresponding to \code{categories}
#' @slot overall data.frame containing the overall agreement budget
#' @slot category list of data.frames showing the agreement budget for each
#'   category
#' @slot transition list of data.frames showing the agreement budget for all
#'   possible transitions
#'
#' @export
#' @exportClass AgreementBudget
#' @rdname AgreementBudget-class

setClass("AgreementBudget",
         contains = c("ThreeMapComparison"),
         slots = c(overall = "data.frame",
                   category = "list",
                   transition = "list"),
         validity = function(object) {
             ## TODO
             return(TRUE)
         }
)

#' Class FigureOfMerit
#'
#' An S4 class for different figure of merit scores.
#'
#' @slot tables list of data.frames that depict the three dimensional table
#'   described by Pontius et al. (2011) at different resolutions
#' @slot factors numeric vector of aggregation factors
#' @slot maps list of RasterStack objects containing land use maps at different
#'   resolutions
#' @slot categories numeric vector of land use categories
#' @slot labels character vector corresponding to \code{categories}
#' @slot overall list containing the overall figure of merit score for each
#'   aggregation factor
#' @slot category list of numeric vectors containing category specific scores
#' @slot transition list of matrices containing transition specific scores
#' 
#' @export
#' @exportClass FigureOfMerit
#' @rdname FigureOfMerit-class
setClass("FigureOfMerit",
         contains = c("ThreeMapComparison"),
         slots = c(overall = "list",
                   category = "list",
                   transition = "list"),
         validity = function(object) {
             ## TODO
             return(TRUE)
         }
)
