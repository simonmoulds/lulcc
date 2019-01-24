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
#'   \item rule == 0 | rule == 1: this rule concerns specific land use
#'     transitions that are allowed (1) or not (0)
#'   \item rule > 100 & rule < 1000: this rule imposes a time limit (rule - 100)
#'     on land use transitions, after which land use change is not allowed. Time
#'     is taken from \code{hist}
#'   \item rule > 1000: this rule imposes a minimum period of time (rule-1000)
#'     before land use is allowed to change
#' }
#'
#' \code{allow} should be called from \code{\link{allocate}} methods. The output
#' is a matrix with the same dimensions as the matrix used internally by
#' allocation functions to store land use suitability. Thus, by multiplying the
#' two matrices together, disallowed transitions are removed from the allocation
#' procedure.
#'
#' @param x numeric vector containing the land use pattern for the current
#'   timestep
#' @param categories numeric vector containing land use categories in the study
#'   region
#' @param cd numeric vector indicating the direction of change for each
#'   land use category. A value of 1 means demand is increasing (i.e. the number
#'   of cells belonging to the category must increase), -1 means decreasing
#'   demand and 0 means demand is static 
#' @param rules matrix. See details
#' @param hist numeric vector containing land use history (values represent the
#'   number of timesteps the cell has contained the current land use category).
#'   Only required for rules 2 and 3
#' @param \dots additional arguments (none)
#'
#' @return A matrix.
#'
#' @useDynLib lulcc
#'
#' @seealso \code{\link{allowNeighb}}
#'
#' @export
#' @references Verburg, P.H., Soepboer, W., Veldkamp, A., Limpiada, R., Espaldon,
#' V., Mastura, S.S. (2002). Modeling the spatial dynamics of regional land use:
#' the CLUE-S model. Environmental management, 30(3):391-405.
#'
#' @examples
#'
#' ## Plum Island Ecosystems
#'
#' ## load observed land use data
#' obs <- ObsLulcRasterStack(x=pie,
#'                    pattern="lu",
#'                    categories=c(1,2,3),
#'                    labels=c("forest","built","other"),
#'                    t=c(0,6,14))
#' 
#' ## get land use values
#' x <- getValues(obs[[1]])
#' x <- x[!is.na(x)]
#' 
#' ## create vector of arbitrary land use history values
#' hist <- sample(1:10, length(x), replace=TRUE)
#' 
#' ## calculate demand and get change direction for first timestep
#' dmd <- approxExtrapDemand(obs=obs, tout=0:14)
#' cd <- dmd[2,] - dmd[1,]
#'  
#' ## create rules matrix, only allowing forest to change if the cell has
#' ## belonged to forest for more than 8 years
#' rules <- matrix(data=c(1,1008,1008,
#'                         1,1,1,
#'                         1,1,1), nrow=3, ncol=3, byrow=TRUE)
#' 
#' allow <- allow(x=x,
#'                hist=hist,
#'                categories=obs@@categories,
#'                cd=cd,
#'                rules=rules)
#' 
#' ## create raster showing cells that are allowed to change from forest to built
#' r <- obs[[1]]
#' r[!is.na(r)] <- allow[,2]
#' r[obs[[1]] != 1] <- NA
#' plot(r)
#' 
#' ## NB output is only useful when used within allocation routine
#'

allow <- function(x, categories, cd, rules, hist=NULL, ...) {
    if (!all(dim(rules) %in% length(categories))) stop("rules matrix should be a square matrix with dimension equal to number of categories")
    if (length(cd) != length(categories)) stop("cd must correspond with categories")
    if (length(x) == 0) stop("x contains no values")
    if (!is.null(hist) && length(x) != length(hist)) stop("hist must have the same length as x")
    if (is.null(hist) && any(rules > 99)) stop("a map of land use history is required for these rules")
    if (is.null(hist)) hist <- rep(1, length(x))
    ## TODO: change C function so hist is optional
    
    rules <- t(rules) ## this makes it easier to handle in C function
    allow <- matrix(data=NA, nrow=length(x), ncol=length(categories))
    allow[] <- .Call("allow", x, hist, categories, cd, rules)
    allow[allow == 0] <- NA
    allow
}
