#' Create a FigureOfMerit object
#'
#' Calculate the figure of merit at different levels and at different resolutions
#' for a reference map at time 1, a reference map at time 2 and a simulated map
#' at time 2.
#'
#' In land use change modelling terms, the figure of merit is the intersection
#' of observed change and simulated change divided by the union of these, with
#' a range of 0 (perfect disagreement) to 1 (perfect agreement). It is useful to
#' to calculate the figure of merit at three levels: (1) considering all possible
#' transitions from all land use categories, (2) considering all transitions from
#' specific land use categories and (3) considering a specific transition from
#' one land use category to another.
#'
#' @param x a \code{ThreeMapComparison} object
#' @param \dots additional arguments (none)
#'
#' @seealso \code{\link{ThreeMapComparison}},\code{\link{FigureOfMerit.plot}}
#' @return A \code{FigureOfMerit} object.
#'
#' @export
#'
#' @references Pontius Jr, R.G., Peethambaram, S., Castella, J.C. (2011).
#' Comparison of three maps at multiple resolutions: a case study of land change
#' simulation in Cho Don District, Vietnam. Annals of the Association of American
#' Geographers 101(1): 45-62.

## NB Equation numbers refer to those in Pontius et al. (2011)
FigureOfMerit <- function(x, ...) {

    ## retrieve information from x
    categories <- x@categories
    labels <- x@labels
    factors <- x@factors
    tables <- x@tables 

    overall.list <- list()
    category.list <- list()
    transition.list <- list()

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

        overall.list[[f]] <- (a - b) / (1 - b) ## Equation 9

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
        category.list[[f]] <- eq10

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
        transition.list[[f]] <- eq11
    }

    out <- new("FigureOfMerit",
               overall=overall.list,
               category=category.list,
               transition=transition.list,
               factors=factors,
               categories=categories,
               labels=labels)

}
