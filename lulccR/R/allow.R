#' Implement decision rules for land use change
#'
#' Identify legitimate transitions based on land use history and specific
#' transition rules. 
#'
#' Decision rules are based on those described by Verburg et al. (2002). The
#' \code{rules} input argument is a square matrix with dimensions equal to the
#' number of land use categories in the study region where rows represent the
#' current land use and columns represent future transitions. The value of each
#' element should represent a rule from the following list:
#'
#' \enumerate{
#'   \item (rule == 0 | rule == 1): this rule concerns specific land use
#'     transitions that are allowed (1) or not (0)
#'   \item (rule == -1): this rule prevents transitions unless demand
#'     for the present land use category is decreasing
#'   \item (rule == -2): this rule prevents transitions to land use
#'     categories with decreasing or static demand. Note that by combining
#'     rule 3 and rule 2 it is possible to prevent simultaneous expansion and
#'     contraction
#'   \item (rule > 100 & rule < 1000): this rule imposes a time limit (rule-100)
#'     on land use transitions, after which land use change is not allowed. Time
#'     is taken from \code{hist}
#'   \item (rule > 1000): this rule imposes a minimum period of time (rule-1000)
#'     before land use is allowed to change
#' }
#'
#' \code{allow} should be called from methods in \code{\link{allocate}}. The
#' output is a matrix with the same dimensions as the matrix used internally by
#' allocation functions to store land use suitability. Thus, by multiplying the
#' two matrices together, disallowed transitions can be removed from the
#' allocation procedure.
#'
#' @param x numeric vector containing the land use pattern for the current
#'   timestep
#' @param hist numeric vector containing land use history (values represent the
#'   number of years the cell has contained the current land use category)
#' @param categories numeric vector containing land use categories in the study
#'   region
#' @param cd numeric vector indicating the direction of change for each
#'   land use category. A value of 1 means demand is increasing (i.e. the number
#'   of cells belonging to the category must increase), -1 means decreasing
#'   demand and 0 means demand is static 
#' @param rules matrix. See details
#' @param ... additional arguments (none)
#'
#' @author Simon Moulds
#' @return a matrix with values of 1 (change allowed) or NA (change not allowed)
#'
#' @useDynLib lulccR
#' @export
#' @references Verburg, P.H., Soepboer, W., Veldkamp, A., Limpiada, R., Espaldon,
#' V., Mastura, S.S. (2002). Modeling the spatial dynamics of regional land use:
#' the CLUE-S model. Environmental management, 30(3):391-405.
#'
#' @examples
#'
#' obs <- ObservedMaps(x=pie,
#'                     pattern="lu",
#'                     categories=c(1,2,3),
#'                     labels=c("forest","built","other"),
#'                     t=c(0,6,14))
#'
#' # create arbitrary land use history raster
#' hist <- raster(obs@@maps[[1]])
#' vals <- sample(1:10, ncell(hist))
#' hist <- setValues(hist, vals[which(!is.na(getValues(obs@@maps[[1]])))]
#'
#' # calculate demand and get change direction for first timestep
#' dmd <- approxExtrapDemand(obs=obs, tout=0:14)
#' cd <- dmd[2,] - dmd[1,]
#' 
#' # create rules matrix, only allowing forest to change if the cell has
#' # belonged to forest for more than 8 years
#' rules <- matrix(data=c(1,1008,1008,
#'                        1,1,1,
#'                        1,1,1), nrow=3, ncol=3, byrow=TRUE)
#'
#' allow <- allow(x=obs@@maps[[1]],
#'                hist=hist,
#'                categories=obs@@categories,
#'                cd=cd,
#'                rules=rules)
#'
#' # NB output is only useful when used within an allocation routine

allow <- function(x, hist, categories, cd, rules, ...) {
    if (!all(dim(rules) %in% length(categories))) stop("rules matrix should be a square matrix with dimension equal to number of categories")
    if (length(cd) != length(categories)) stop("cd must correspond with categories")
    if (length(x) != length(hist)) stop("hist must correspond with x")
    if (length(x) == 0) stop("x contains no values")
    rules <- t(rules)
    allow <- matrix(data=NA, nrow=length(x), ncol=length(categories))
    allow[] <- .Call("allow", x, hist, categories, cd, rules)
    allow[allow == 0] <- NA
    allow
}

## if ( !isGeneric("allow")) {
##     setGeneric("allow", function(lu, hist, categories, cd, rules, ...)
##                    standardGeneric("allow"))
## }

## setMethod("allow", signature(lu = "numeric", hist = "numeric", categories = "numeric", cd = "numeric", rules = "matrix"),
##           function(lu, hist, categories, cd, rules, ...) {
##               if (!all(dim(rules) %in% length(categories))) stop("rules matrix should be a square matrix with dimension equal to number of land use categories")
##               if (length(cd) != length(categories)) stop("cd must have the same length as categories")
##               if (length(lu) != length(hist)) stop("hist cells do not correspond with lu cells")
##               if (length(lu) == 0) stop("lu contains no values")
##               rules <- t(rules)
##               allow <- matrix(data=NA, nrow=length(lu), ncol=length(categories))
##               allow[] <- .Call("allow", lu, hist, categories, cd, rules)
##               allow[allow == 0] <- NA
##               return(allow)
##           }
## )
