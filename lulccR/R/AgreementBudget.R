#' Create an AgreementBudget object
#' 
#' This function quantifies sources of agreement and disagreement between a
#' reference map for time 1, a reference map for time 2 and a simulated map for
#' time 2 to provide meaningful information about the performance of land use
#' change simulations.
#'
#' The types of agreement and disagreement considered are as follows:
#'
#' \enumerate{
#'   \item Persistence simulated correctly (agreement)
#'   \item Persistence simulated as change (disagreement)
#'   \item Change simulated incorrectly (disagreement)
#'   \item Change simulated correctly (agreement)
#'   \item Change simulated as persistence (disagreement)
#' }
#'
#' @param x a \code{ThreeMapComparison} object
#' @param from numeric (optional). A single value representing a land use
#'   category. Results will be restricted to transitions from this category
#' @param to numeric (optional). Similar to \code{from}. If provided with a valid
#'   \code{from} argument the result will be restricted to the transition
#'   defined by these two arguments (i.e. \code{from} -> \code{to})
#' @param ... additional arguments (none) 
#'
#' @seealso \code{link{ThreeMapComparison}}, \code{link{FigureOfMerit}}
#' @author Simon Moulds
#' @return an \code{AgreementBudget} object
#'
#' @export

AgreementBudget <- function(x, from, to, ...) {

    categories <- x@categories
    labels <- x@labels
    factors <- x@factors
    tables <- x@tables

    if (!missing(from) && missing(to)) type <- "category"

    if (!missing(from)) {
        from.ix <- which(categories %in% from)
        if (length(from.ix) > 1) {
            stop("'from' must be a single land use category")
        } else if (length(from.ix) == 0) {
            stop("'from' is not a valid land use category")
        }

    } else {
        type <- "overall"
        from <- categories
        from.ix <- 1:length(categories)
    }

    if (!missing(from) && !missing(to)) {
        to.ix <- which(categories %in% to)
        if (length(to.ix) > 1) {
            stop("'to' must be a single land use category")
        } else if (length(to.ix) == 0) {
            stop("'to' is not a valid land use category")
        }
        if (from.ix == to.ix) stop("'from' cannot equal 'to'")
        type <- "transition"        
    } else {
        to <- categories
        to.ix <- 1:length(categories)
    }   

    ## number of sources of agreement/disagreement (correct persistence not possible with 'transition')
    if (type == "transition") {
        n <- 4
    } else {
        n <- 5
    }

    ## preallocate output data.frame
    agreement <- as.data.frame(matrix(data=NA, nrow=length(factors), ncol=n))
    names(agreement) <- c("a","b","c","d","e")[1:n]

    for (f in 1:length(x@tables)) {
        tab <- x@tables[[f]]

        ## change simulated as persistence
        a <- rep(0, length(categories))
        for (j in to.ix) {
            asub <- rep(0, length(categories))
            for (i in from.ix) {
                if (i != j) {
                    ixy <- i + (j-1) * (length(categories) + 1)
                    ixx <- i
                    asub[i] <- tab[ixy,ixx]
                }
            }
            a[j] <- sum(asub, na.rm=TRUE)
        }
        a <- sum(a)

        ## correctly simulated change
        b <- rep(0, length(categories))
        for (j in to.ix) {
            bsub <- rep(0, length(categories))
            for (i in from.ix) {
                if (i != j) {
                    ixy <- i + (j-1) * (length(categories) + 1)
                    ixx <- j
                    bsub[i] <- tab[ixy,ixx]
                } 
            }
            b[j] <- sum(bsub)
        }
        b <- sum(b)

        ## incorrectly simulated change
        c <- rep(0, length(categories)) 
        for (j in to.ix) {
            csub <- rep(0, length(categories))   
            for (i in from.ix) {
                csubsub <- rep(0, length(categories))               
                for (k in 1:length(categories)) {
                    if (i != j && i != k && j != k) {
                        ixy <- i + (j-1) * (length(categories) + 1)
                        ixx <- k
                        csubsub[k] <- tab[ixy,ixx]
                    }
                }
                csub[i] <- sum(csubsub, na.rm=TRUE)
            }
            c[j] <- sum(csub, na.rm=TRUE)
        }
        c <- sum(c)

        ## if looking at specific transitions...
        if (length(to.ix) == 1) {
            csub <- rep(0, length(categories))
            for (j in 1:length(categories)) {
                if (j != from.ix && j != to.ix) {
                    ixy <- from.ix + (j-1) * (length(categories) + 1)
                    ixx <- to.ix
                    csub[j] <- tab[ixy,ixx]
                }
            }
            c <- c + sum(csub)
        }

        ## persistence simulated incorrectly
        d <- rep(0, length(categories))
        for (j in from.ix) {
            dsub <- rep(0, length(categories))
            for (k in to.ix) {
                if (k != j) {
                    ixy <- j + (j-1) * (length(categories) + 1)
                    ixx <- k 
                    dsub[k] <- tab[ixy,ixx]
                }
            }
            d[j] <- sum(dsub)
        }
        d <- sum(d)

        ## persistence simulated correctly
        if (type == "overall" || type == "category") {
            e <- rep(0, length(categories))
            for (i in from.ix) {
                ixy <- i + (i-1) * (length(categories) + 1)
                ixx <- i
                e[i] <- tab[ixy,ixx]
            }
            e <- sum(e)
            agreement[f,] <- c(a, b, c, d, e)
        } else {
            agreement[f,] <- c(a, b, c, d)
        }

    }

    out <- new("AgreementBudget",
               agreement=agreement,
               factors=factors,
               type=type,
               categories=categories,
               labels=labels,
               from=from,
               to=to)

}

