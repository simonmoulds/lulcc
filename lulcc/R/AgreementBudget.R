#' @include class-ThreeMapComparison.R
NULL

#' Create an AgreementBudget object
#' 
#' This function quantifies sources of agreement and disagreement between a
#' reference map for time 1, a reference map for time 2 and a simulated map for
#' time 2 to provide meaningful information about the performance of land use
#' change simulations.
#'
#' The types of agreement and disagreement considered are those descibed in
#' Pontius et al. (2011):
#'
#' \enumerate{
#'   \item Persistence simulated correctly (agreement)
#'   \item Persistence simulated as change (disagreement)
#'   \item Change simulated incorrectly (disagreement)
#'   \item Change simulated correctly (agreement)
#'   \item Change simulated as persistence (disagreement)
#' }
#'
#' @param x a ThreeMapComparison object or RasterLayer
#' @param \dots additional arguments to ThreeMapComparison. Only required if x is
#'   not a ThreeMapComparison object
#'
#' @seealso \code{\link{AgreementBudget-class}},
#' \code{\link{plot.AgreementBudget}}, \code{\link{ThreeMapComparison}},
#' \code{\link{FigureOfMerit}} 
#'
#' @return An \code{AgreementBudget} object.
#'
#' @export
#' @rdname AgreementBudget
#' 
#' @references Pontius Jr, R.G., Peethambaram, S., Castella, J.C. (2011).
#' Comparison of three maps at multiple resolutions: a case study of land change
#' simulation in Cho Don District, Vietnam. Annals of the Association of American
#' Geographers 101(1): 45-62.
#'
#' @examples
#'
#' ## see lulcc-package examples

setGeneric("AgreementBudget", function(x, ...)
           standardGeneric("AgreementBudget"))

#' @rdname AgreementBudget
#' @aliases AgreementBudget,ThreeMapComparison-method
setMethod("AgreementBudget", signature(x = "ThreeMapComparison"),
          function(x, ...) {

              ## overall
              overall.agr <- .agreementBudget(tables=x@tables,
                                              factors=x@factors,
                                              categories=x@categories,
                                              type="overall",
                                              from.ix=1:length(x@categories),
                                              to.ix=1:length(x@categories))

              ## category
              category.agr <- list()
              for (i in 1:length(x@categories)) {
                  category.agr[[i]] <- .agreementBudget(tables=x@tables,
                                                        factors=x@factors,
                                                        type="category",
                                                        categories=x@categories,
                                                        from.ix=i,
                                                        to.ix=1:length(x@categories))
              }
              names(category.agr) <- x@labels

              ## transition
              transition.agr <- list()
              for (i in 1:length(x@categories)) {
                  for (j in 1:length(x@categories)) {
                      ix <- (i-1) * length(x@categories) + j
                      transition.agr[[ix]] <- .agreementBudget(tables=x@tables,
                                                               factors=x@factors,
                                                               type="transition",
                                                               categories=x@categories,
                                                               from.ix=i,
                                                               to.ix=j)

                  }
              }
              names(transition.agr) <- paste0(rep(x@labels, each=length(x@categories)), "-", rep(x@labels, length(x@categories)))

              new("AgreementBudget", x, overall=overall.agr, category=category.agr, transition=transition.agr)
          }
)

#' @rdname AgreementBudget
#' @aliases AgreementBudget,RasterLayer-method
setMethod("AgreementBudget", signature(x = "RasterLayer"),
          function(x, ...) {
              x <- ThreeMapComparison(x, ...)
              agreement <- AgreementBudget(x)
          }
)

.agreementBudget <- function(tables, factors, categories, type, from.ix=NA, to.ix=NA) {
    
    ## number of sources of agreement/disagreement (correct persistence not possible with 'transition')
    if (type == "transition") {
        n <- 4
    } else {
        n <- 5
    }
    
    ## preallocate output data.frame
    agreement <- as.data.frame(matrix(data=NA, nrow=length(factors), ncol=n))
    names(agreement) <- c("a","b","c","d","e")[1:n]

    for (f in 1:length(tables)) {
        tab <- tables[[f]]

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
    
    agreement

}
