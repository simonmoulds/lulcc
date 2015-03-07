#' Extrapolate land use area in time
#'
#' Extrapolate land use area from two or more observed land use maps to provide
#' a valid (although not necessarily realistic) demand scenario.
#'
#' Many allocation routines, including the two included with \code{lulccR},
#' require non-spatial estimates of land use demand for every timestep in the
#' study period. Some routines are coupled to complex economic models that
#' predict future or past land use demand based on economic considerations;
#' however, linear extrapolation of trends remains a useful technique.
#'
#' @param obs an \code{ObsLulcMaps} object containing at least two maps
#' @param tout numeric vector specifying the timesteps where interpolation is to
#'   take place. Comparable to the \code{xout} argument of
#'   \code{Hmisc::\link[Hmisc]{approxExtrap}}
#' @param \dots additional arguments to \code{Hmisc::\link[Hmisc]{approxExtrap}}
#'
#' @return A matrix where columns correspond to land use categories and rows
#'   correspond to timesteps.
#'
#' @export
#'
#' @examples
#'
#' ## Plum Island Ecosystems
#'
#' ## load observed land use maps
#' obs <- ObsLulcMaps(x=pie,
#'                     pattern="lu",
#'                     categories=c(1,2,3),
#'                     labels=c("forest","built","other"),
#'                     t=c(0,6,14))
#'
#' ## obtain demand scenario
#' dmd <- approxExtrapDemand(obs=obs, tout=c(0:14))

approxExtrapDemand <- function(obs, tout, ...) {
    if (nlayers(obs@maps) > 1) {
        demand <- matrix(data=NA, nrow=length(tout), ncol=length(obs@categories))
        for (i in 1:length(obs@categories)) {
            x <- Hmisc::approxExtrap(obs@t, obs@total[,i], tout)$y
            x[x < 0] <- 0
            demand[,i] <- x
        }

    } else {
        stop("cannot estimate land use demand with only one observed map")
    }

    ncell <- length(which(!is.na(raster::getValues(obs@maps[[1]]))))
    demand <- roundSum(demand, ncell)
}

#' Round elements in matrix or data.frame rows
#'
#' Round all numbers in a matrix or data.frame while ensuring that all rows sum
#' to the same value.
#'
#' The main application of \code{roundSum} is to ensure that each row in the
#' demand matrix specifies exactly the number of cells to be allocated to each
#' land use category for the respective timestep. It may also be used to convert
#' the units of demand to number of cells, as required by \code{ModelInput}.
#'
#' @param x matrix or data.frame
#' @param ncell numeric specifying the target sum for each row in \code{x}
#' @param \dots additional arguments (none)
#'
#' @return A matrix.
#'
#' @export
#'
#' @examples
#'
#' ## Sibuyan Island
#'
#' ## load demand scenario from data
#' dmd <- sibuyan$demand$demand1 * runif(1)
#' ncell <- length(which(!is.na(getValues(sibuyan$maps$lu_sib_1997))))
#'
#' ## recover demand
#' dmd <- roundSum(dmd, ncell=ncell)

roundSum <- function(x, ncell, ...) {
    
    if (missing(x)) stop("missing 'x'") 
    if (missing(ncell)) stop("missing 'ncell'") 
    
    for (i in 1:nrow(x)) {
        y <- x[i,]
        y <- y / sum(y) * ncell ## scale row to ensure it sums to ncell
        xint <- floor(y) ## convert x to integer
        diff <- y - floor(y) ## roundoff error TODO: tolerance?
        diff <- sort(diff, index.return=TRUE) ## sort diff by roundoff error
        tot.diff <- ncell - sum(floor(y))
        if (tot.diff > 0) {
            ix <- seq((length(y)-tot.diff+1), length(y), 1)
            ix <- diff$ix[ix]
            xint[ix] <- xint[ix] + 1
        }
        x[i,] <- xint
    }
    x
}
