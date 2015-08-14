#' @include class-ThreeMapComparison.R
NULL

#' Create a FigureOfMerit object
#'
#' Calculate the figure of merit at different levels and at different resolutions
#' for a reference map at time 1, a reference map at time 2 and a simulated map
#' at time 2.
#'
#' In land use change modelling the figure of merit is the intersection of
#' observed change and simulated change divided by the union of these, with a
#' range of 0 (perfect disagreement) to 1 (perfect agreement). It is useful to
#' calculate the figure of merit at three levels: (1) considering all possible
#' transitions from all land use categories, (2) considering all transitions from
#' specific land use categories and (3) considering a specific transition from
#' one land use category to another.
#'
#' @param x a ThreeMapComparison object or RasterLayer
#' @param \dots additional arguments to ThreeMapComparison. Only required if x is
#'   not a ThreeMapComparison object
#'
#' @seealso \code{\link{plot.FigureOfMerit}}, \code{\link{ThreeMapComparison}}
#' @return A FigureOfMerit object.
#'
#' @export
#' @rdname FigureOfMerit
#'
#' @references Pontius Jr, R.G., Peethambaram, S., Castella, J.C. (2011).
#' Comparison of three maps at multiple resolutions: a case study of land change
#' simulation in Cho Don District, Vietnam. Annals of the Association of American
#' Geographers 101(1): 45-62.
#'
#' @examples
#'
#' ## see lulcc-package examples

setGeneric("FigureOfMerit", function(x, ...)
           standardGeneric("FigureOfMerit"))

#' @rdname FigureOfMerit
#' @aliases FigureOfMerit,RasterLayer-method
setMethod("FigureOfMerit", signature(x = "RasterLayer"),
          function(x, ...) {
              x <- ThreeMapComparison(x, ...)
              fom <- FigureOfMerit(x)
          }
)

#' @rdname FigureOfMerit
#' @aliases FigureOfMerit,ThreeMapComparison-method
setMethod("FigureOfMerit", signature(x = "ThreeMapComparison"),
          function(x, ...) {
              fom <- .figureOfMerit(tables=x@tables, factors=x@factors, categories=x@categories)
              fom <- new("FigureOfMerit", x, overall=fom$overall, category=fom$category, transition=fom$transition)
          }
)

## NB Equation numbers refer to those in Pontius et al. (2011)
.figureOfMerit <- function(tables, factors, categories) {

    overall.fom <- list()
    category.fom <- list()
    transition.fom <- list()

    for (f in 1:length(factors)) {

        tab <- tables[[f]]

        ## Equation 9 (overall figure of merit)
        a <- 0
        b <- 0            
        for (j in 1:length(categories)) {
            a.ixy <- j * (length(categories) + 1)
            a.ixx <- j
            a <- sum(c(a, tab[a.ixy, a.ixx]), na.rm=TRUE) ## expression left of minus sign in numerator
            b.ixy <- j + (j-1) * (length(categories) + 1)
            b.ixx <- j
            b <- sum(c(b, tab[b.ixy, b.ixx]), na.rm=TRUE) ## expression right of minus sign in numerator
        }

        overall.fom[[f]] <- (a - b) / (1 - b) ## Equation 9

        ## Equation 10 (figure of merit for each category)
        eq10 <- rep(0, length(categories))
        names(eq10) <- categories ## useful?
        for (i in 1:length(categories)) {
            a <- 0
            b <- 0
            for (j in 1:length(categories)) {
                a.ixy <- i + (j-1) * (length(categories) + 1)
                a.ixx <- j
                a <- sum(c(a, tab[a.ixy, a.ixx]), na.rm=TRUE) ## expression left of minus sign in numerator
                b.ixy <- a.ixy
                b.ixx <- length(categories) + 1
                b <- sum(c(b, tab[b.ixy, b.ixx]), na.rm=TRUE) ## expression left of minus sign in denominator 
            }
            c.ixy <- i + (i-1) * (length(categories) + 1)
            c.ixx <- i
            c <- tab[c.ixy, c.ixx] ## expression right of plus sign in numerator
            eq10[i] <- (a - c) / (b - c) ## Equation 10
        }
        category.fom[[f]] <- eq10

        ## Equation 11 (figure of merit for each transition)
        eq11 <- matrix(data=0, nrow=length(categories), ncol=length(categories))
        colnames(eq11) <- categories ## useful?
        rownames(eq11) <- categories ## useful?
        for (j in 1:length(categories)) {
            for (i in 1:length(categories)) {
                a.ixy <- i + (j-1) * (length(categories) + 1)
                a.ixx <- j 
                a <- tab[a.ixy, a.ixx] ## numerator
                b.ixy <- i + (j-1) * (length(categories) + 1)
                b.ixx <- length(categories) + 1
                b <- tab[b.ixy, b.ixx] ## expression left of plus sign in denominator
                c <- 0
                for (k in 1:length(categories)) {
                    c.ixy <- i + (k-1) * (length(categories) + 1)
                    c.ixx <- j
                    c <- sum(c(c, tab[c.ixy, c.ixx]), na.rm=TRUE) ## expression right of plus sign in denominator
                }
                eq11[i,j] <- a / (b + c - a) ## Equation 11
            }
        }
        transition.fom[[f]] <- eq11
    }

    list(overall=overall.fom, category=category.fom, transition=transition.fom)

}

    
