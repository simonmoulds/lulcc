#' Extrapolate land use area in time
#'
#' Extrapolate land use area from two or more observed land use maps to provide
#' a valid (although not necessarily realistic) demand scenario.
#'
#' Many allocation routines, including the two included with \code{lulcc},
#' require non-spatial estimates of land use demand for every timestep in the
#' study period. Some routines are coupled to complex economic models that
#' predict future or past land use demand based on economic considerations;
#' however, linear extrapolation of trends remains a useful technique.
#'
#' @param obs an ObsLulcRasterStack object containing at least two maps
#' @param tout numeric vector specifying the timesteps where interpolation is to
#'   take place. Comparable to the \code{xout} argument of
#'   \code{Hmisc::\link[Hmisc]{approxExtrap}}
#' @param \dots additional arguments to \code{Hmisc::\link[Hmisc]{approxExtrap}}
#'
#' @return A matrix.
#'
#' @seealso \code{Hmisc::\link[Hmisc]{approxExtrap}}
#'
#' @export
#'
#' @examples
#'
#' ## Plum Island Ecosystems
#'
#' ## load observed land use maps
#' obs <- ObsLulcRasterStack(x=pie,
#'                    pattern="lu",
#'                    categories=c(1,2,3),
#'                    labels=c("forest","built","other"),
#'                    t=c(0,6,14))
#' 
#' ## obtain demand scenario by interpolating between observed maps
#' dmd <- approxExtrapDemand(obs=obs, tout=c(0:14))
#' 
#' ## plot
#' matplot(dmd, type="l", ylab="Demand (no. of cells)", xlab="Time point",
#'         lty=1, col=c("Green","Red","Blue"))
#' legend("topleft", legend=obs@@labels, col=c("Green","Red","Blue"), lty=1)
#' 
#' ## linear extrapolation is also possible
#' dmd <- approxExtrapDemand(obs=obs, tout=c(0:50))
#' 
#' ## plot
#' matplot(dmd, type="l", ylab="Demand (no. of cells)", xlab="Time point",
#'         lty=1, col=c("Green","Red","Blue"))
#' legend("topleft", legend=obs@@labels, col=c("Green","Red","Blue"), lty=1)
#' 

approxExtrapDemand <- function(obs, tout, ...) {
    if (nlayers(obs) > 1) {
        tot <- total(x=obs)$total
        demand <- matrix(data=NA, nrow=length(tout), ncol=length(obs@categories))
        for (i in 1:length(obs@categories)) {
            x <- Hmisc::approxExtrap(obs@t, tot[,i], tout)$y
            x[x < 0] <- 0
            demand[,i] <- x
        }

    } else {
        stop("cannot estimate land use demand with only one observed map")
    }

    ncell <- length(which(!is.na(raster::getValues(obs[[1]]))))
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
#' the units of demand to number of cells.
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
#' ## load observed land use data and create demand scenario
#' obs <- ObsLulcRasterStack(x=sibuyan$maps,
#'                     pattern="lu",
#'                     categories=c(1,2,3,4,5),
#'                     labels=c("Forest","Coconut","Grass","Rice","Other"),
#'                     t=c(0,14))
#' 
#' dmd <- approxExtrapDemand(obs, tout=0:14)
#' apply(dmd, 1, sum)
#' 
#' ## artificially perturb for illustration purposes
#' dmd <- dmd * runif(1)
#' apply(dmd, 1, sum)
#' 
#' ## use roundSum to correct demand scenario
#' ncell <- length(which(!is.na(getValues(sibuyan$maps$lu_sib_1997))))
#' ncell
#' dmd <- roundSum(dmd, ncell=ncell)
#' apply(dmd, 1, sum)
#'

roundSum <- function(x, ncell, ...) {
    
    if (missing(x)) stop("missing 'x'") 
    if (missing(ncell)) stop("missing 'ncell'") 
    
    for (i in 1:nrow(x)) {
        y <- as.numeric(x[i,])
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
