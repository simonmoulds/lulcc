#' Implement neighbourhood decision rules
#'
#' Identify legitimate transitions for each cell according to neighbourhood
#' decision rules.
#' 
#' @param neighb a NeighbRasterStack object
#' @param x a categorical RasterLayer to which neighbourhood rules should be
#'   applied. If \code{neighb} is supplied it is updated with this map
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
#' @return A matrix.
#'
#' @seealso \code{\link{allow}}, \code{\link{NeighbRasterStack}}
#' 
#' @export
#' @examples
#'
#' ## Plum Island Ecosystems
#'
#' ## load observed land use data
#' obs <- ObsLulcRasterStack(x=pie,
#'                      pattern="lu",
#'                      categories=c(1,2,3),
#'                      labels=c("forest","built","other"),
#'                      t=c(0,6,14))
#' 
#' ## create a NeighbRasterStack object for forest only
#' w <- matrix(data=1, nrow=3, ncol=3)
#' nb <- NeighbRasterStack(x=obs[[1]], weights=w, categories=1)
#' 
#' ## only allow change to forest within neighbourhood of current forest cells
#' ## note that rules can be any value between zero (less restrictive) and one
#' ## (more restrictive)
#' nb.allow <- allowNeighb(neighb=nb,
#'                         x=obs[[1]],
#'                         categories=obs@@categories,
#'                         rules=0.5)
#' 
#' ## create raster showing cells allowed to change to forest
#' r <- obs[[1]]
#' r[!is.na(r)] <- nb.allow[,1]
#' plot(r)
#' 
#' ## NB output is only useful when used within an allocation routine
#'

allowNeighb <- function(neighb, x, categories, rules, ...) {
    if (length(rules) != nlayers(neighb)) stop("rule should be provided for each neighbourhood map")
    cells <- which(!is.na(raster::getValues(x)))
    neighb <- NeighbRasterStack(x=x, neighb=neighb) ## update neighbourhood maps
    allow.nb <- matrix(data=1, nrow=length(cells), ncol=length(categories))
    for (i in 1:length(neighb@categories)) {
        ix <- which(categories %in% neighb@categories[i])
        nb.vals <- raster::extract(neighb[[i]], cells)
        allow.nb[,ix] <- as.numeric(nb.vals >= rules[i])
    }    
    allow.nb[allow.nb == 0] <- NA
    allow.nb
}

