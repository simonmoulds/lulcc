#' Implement neighbourhood decision rules
#'
#' Identify legitimate transitions for each cell according to neighbourhood
#' decision rules.
#'
#' See \code{\link{allow}}.
#' 
#' @param x a \code{NeighbMaps} object
#' @param cells index of non-NA cells in the study region
#' @param categories numeric vector containing land use categories. If
#'   \code{allowNeighb} is called from an allocation model this argument
#'   should contain all categories in the simulation, regardless of whether
#'   they're associated with a neighbourhood decision rule
#' @param rules a numeric vector with neighbourhood decision rules. Each rule
#'   is a value between 0 and 1 representing the threshold neighbourhood value
#'   above which change is allowed. Rules should correspond with
#'   \code{x@@categories}
#' @param \dots additional arguments (none)
#'
#' @return a matrix. See \code{\link{allow}}.
#' 
#' @export
#' @examples
#'
#' # observed data
#' obs <- ObservedMaps(x=pie,
#'                     pattern="lu",
#'                     categories=c(1,2,3),
#'                     labels=c("forest","built","other"),
#'                     t=c(0,6,14))
#'
#' # create a NeighbMaps object
#' nb <- NeighbMaps(x=obs@@maps[[1]],
#'                  categories=1,
#'                  weights=3,
#'                  fun=mean)
#'
#' # index of non-NA cells in study region
#' na.ix <- which(!is.na(getValues(obs@@maps[[1]])))
#'
#' # only allow change to forest within neighbourhood of current forest cells
#' # note that rules can be any value between zero (less restrictive) and one
#' # (more restrictive)
#' nb.allow <- allowNeighb(x=nb,
#'                         cells=na.ix,
#'                         categories=obs@@categories,
#'                         rules=0.5)
#'
#' # NB output is only useful when used within an allocation routine

allowNeighb <- function(x, cells, categories, rules, ...) {
    if (length(rules) != length(x@maps)) stop("rule should be provided for each xourhood map")
    allow.nb <- matrix(data=1, nrow=length(cells), ncol=length(categories))
    for (i in 1:length(x@categories)) {
        ix <- which(categories %in% x@categories[i])
        nb.vals <- raster::extract(x@maps[[i]], cells)
        allow.nb[,ix] <- as.numeric(nb.vals >= rules[i])
    }
    allow.nb[allow.nb == 0] <- NA
    allow.nb
}
