#' Evaluate allocation performance with three maps
#'
#' An implementation of the method described by Pontius et al. (2011), which
#' compares a reference map at time 1, a reference map at time 2 and a simulated
#' map at time 2 to evaluate allocation performance at multiple resolutions while
#' taking into account persistence. The method quantifies disagreement within
#' coarse squares, disagreement between coarse squares, disagreement about the
#' quantity of land use change and agreement.
#'
#' @param rt1 RasterLayer. Observed land use map at time 1
#' @param rt2 RasterLayer. Observed land use map at time 2
#' @param st2 RasterLayer. Simulated land use map at time 2 
#' @param factors numeric vector of aggregation factors (equivalent to the 'fact'
#'   argument to \code{raster::\link[raster]{aggregate}} representing the
#'   resolutions at which model performance should be tested
#' @param ... additional arguments to \code{raster::\link[raster]{aggregate}}
#'
#' @seealso \code{\link{AgreementBudget}}, \code{\link{FigureOfMerit}}
#' @author Simon Moulds
#' @return a \code{ThreeMapComparison} object
#'
#' @export
#'
#' @references Pontius Jr, R.G., Peethambaram, S., Castella, J.C. (2011).
#' Comparison of three maps at multiple resolutions: a case study of land change
#' simulation in Cho Don District, Vietnam. Annals of the Association of American
#' Geographers 101(1): 45-62.

## NB comments refer to equations as written in Pontius et al. (2011)

ThreeMapComparison <- function(rt1, rt2, st2, categories, labels, factors, ...) {

    cr <- raster::compareRaster(rt1, rt2, st2, extent=FALSE, rowcol=FALSE, res=TRUE, tolerance=0.05, stopiffalse=FALSE)
    if (!cr) { stop("resolution and/or CRS of input maps do not agree") }

    ## ensure maps cover exactly the same region by extracting cells based on coordinates of non-NA cells in rt1
    pts.rt1 <- raster::rasterToPoints(rt1, spatial=TRUE)
    rt2.vals <- raster::extract(rt2, pts.rt1)
    st2.vals <- raster::extract(st2, pts.rt1)
    if (length(rt2.vals) != length(pts.rt1)) stop("rt2 contains NA values in study region")
    if (length(st2.vals) != length(pts.rt1)) stop("st2 contains NA values in study region")
    rt2 <- rt1
    st2 <- rt1
    rt2[!is.na(rt2)] <- rt2.vals 
    st2[!is.na(st2)] <- st2.vals 

    ## prepare map to calculate weights
    ones <- rt1
    ones[!is.na(ones)] <- 1

    ## create list to hold output
    tables <- list()
    for (f in 1:length(factors)) {
        
        ## get map of weights
        if (factors[f] > 1) {
            weight <- raster::aggregate(ones, fact=factors[f], fun=sum, na.rm=TRUE, expand=TRUE, ...)
        } else {
            weight <- ones
        }

        wt.vals <- raster::getValues(weight)
        wt.vals <- wt.vals[!is.na(wt.vals)]
        
        ## preallocate three dimensional table
        tab <- matrix(data=NA, nrow=(length(categories) + 1) * (length(categories) + 1), ncol=(length(categories) + 1))
        rt1.list <- list()
        rt2.list <- list()
        st2.list <- list()

        Qng.list <- list()
        Rng.list <- list()
        Sng.list <- list()

        for (j in 1:length(categories)) {
            cat <- categories[j]
            tmp.rt1 <- (rt1 == cat) ## maps with binary values where 1/0 indicates presence/absence of 'cat'
            tmp.rt2 <- (rt2 == cat)
            tmp.st2 <- (st2 == cat)

            if (factors[f] > 1) {
                tmp.rt1 <- raster::aggregate(tmp.rt1, fact=factors[f], fun=sum, na.rm=TRUE, expand=TRUE, ...)
                tmp.rt2 <- raster::aggregate(tmp.rt2, fact=factors[f], fun=sum, na.rm=TRUE, expand=TRUE, ...)
                tmp.st2 <- raster::aggregate(tmp.st2, fact=factors[f], fun=sum, na.rm=TRUE, expand=TRUE, ...)
            }
            
            tmp.rt1.vals <- raster::getValues(tmp.rt1)
            tmp.rt1.vals <- tmp.rt1.vals[!is.na(tmp.rt1.vals)]           
            tmp.rt2.vals <- raster::getValues(tmp.rt2)
            tmp.rt2.vals <- tmp.rt2.vals[!is.na(tmp.rt2.vals)]
            tmp.st2.vals <- raster::getValues(tmp.st2)
            tmp.st2.vals <- tmp.st2.vals[!is.na(tmp.st2.vals)]
                                         
            rt1.list[[j]] <- tmp.rt1.vals
            rt2.list[[j]] <- tmp.rt2.vals
            st2.list[[j]] <- tmp.st2.vals

            Qng.list[[j]] <- tmp.rt1.vals / wt.vals
            Rng.list[[j]] <- tmp.rt2.vals / wt.vals
            Sng.list[[j]] <- tmp.st2.vals / wt.vals
            
        }

        eq1.list <- list()
        eq2.list <- list()
        for (j in 1:length(categories)) {
            eq1.list[[j]] <- pmin(Rng.list[[j]], Sng.list[[j]], na.rm=TRUE) ## Equation 1
            eq2.list[[j]] <- pmin(Qng.list[[j]], Rng.list[[j]], Sng.list[[j]], na.rm=TRUE) ## Equation 2
        }
        
        eq3.list <- list()
        for (j in 1:length(categories)) {

            ## Equation 3
            bsum <- list()
            for (i in 1:length(categories)) {
                if (i != j) {
                    ix <- length(bsum) + 1
                    bsum[[ix]] <- Qng.list[[i]] - eq2.list[[i]]
                }
            }
            bsum <- Reduce("+", bsum) ## denominator of expression right of multiplication sign
            
            eq3.sublist <- list()
            for (i in 1:length(categories)) {
                if (i != j) {
                    b <- Qng.list[[i]] - eq2.list[[i]] ## numerator of expression right of multiplication sign
                    eq3.sublist[[i]] <- (eq1.list[[j]] - eq2.list[[j]]) * b / bsum ## Equation 3
                } else {
                    eq3.sublist[[i]] <- NA
                }
            }
            eq3.list[[j]] <- eq3.sublist
        }

        ## Equation 4
        Qqng.list <- list()
        for (i in 1:length(categories)) {
            Tng <- list()
            for (j in 1:length(categories)) {
                if (i != j) {
                    Tng[[j]] <- eq3.list[[j]][[i]]
                } else {
                    Tng[[j]] <- eq2.list[[i]]
                }
            }
            Tng <- rowSums(do.call(cbind, Tng), na.rm=TRUE)  
            Qqng.list[[i]] <- Qng.list[[i]] - Tng ## Equation 4
            
        }

        ## Equation 5
        Rrng.list <- list()
        for (j in 1:length(categories)) {
            Rrng.list[[j]] <- Rng.list[[j]] - eq1.list[[j]] ## Equation 5
        }

        ## Equation 6
        Ssng.list <- list()
        for (k in 1:length(categories)) {
            Ssng.list[[k]] <- Sng.list[[k]] - eq1.list[[k]] ## Equation 6
        }
        
        ## Equation 7
        a <- Reduce("+", eq1.list) ## expression left of multiplication sign
        bsum <- list()
        for (j in 1:length(categories)) {
            for (k in 1:length(categories)) {
                for (i in 1:length(categories)) {
                    if (j != k) {
                        ix <- length(bsum) + 1
                        bsum[[ix]] <- Qqng.list[[i]] * Rrng.list[[j]] * Ssng.list[[k]]
                    }
                }
            }
        }        
        bsum <- Reduce("+", bsum) ## denominator of expression right of multiplication sign

        eq7.list <- list()
        for (j in 1:length(categories)) {
            eq7.sublist <- list()
            for (k in 1:length(categories)) {
                eq7.subsublist <- list()
                for (i in 1:length(categories)) {
                    if (j != k) {
                        b <- Qqng.list[[i]] * Rrng.list[[j]] * Ssng.list[[k]] ## numerator of expression right of multiplication sign
                        eq7.subsublist[[i]] <- (1 - a) * b / bsum ## Equation 7
                    } else {
                        eq7.subsublist[[i]] <- NA
                    }
                }
                eq7.sublist[[k]] <- eq7.subsublist
            }
            eq7.list[[j]] <- eq7.sublist
        }

        ## Fill in three-dimensional table
        
        ## Rule 1
        for (j in 1:length(categories)) {
            ixy <- j * (length(categories) + 1)
            ixx <- j
            tab[ixy,ixx] <- sum(eq1.list[[j]] * wt.vals, na.rm=TRUE) / sum(wt.vals, na.rm=TRUE)
        }

        ## Rule 2
        for (j in 1:length(categories)) {
            ixy <- j + (j-1) * (length(categories) + 1)
            ixx <- j
            tab[ixy,ixx] <- sum(eq2.list[[j]] * wt.vals, na.rm=TRUE) / sum(wt.vals, na.rm=TRUE)
        }

        ## Rule 3
        for (j in 1:length(categories)) {
            for (i in 1:length(categories)) {
                if (i != j) {
                    ixy <- i + (j-1) * (length(categories) + 1)
                    ixx <- j
                    tab[ixy,ixx] <- sum(eq3.list[[j]][[i]] * wt.vals, na.rm=TRUE) / sum(wt.vals, na.rm=TRUE)
                }
            }
        }
        
        ## Rule 4
        for (j in 1:length(categories)) {
            for (k in 1:length(categories)) {
                for (i in 1:length(categories)) {
                    if (j != k) {
                        ixy <- i + (j-1) * (length(categories) + 1)
                        ixx <- k
                        tab[ixy,ixx] <- sum(eq7.list[[j]][[k]][[i]] * wt.vals, na.rm=TRUE) / sum(wt.vals, na.rm=TRUE)
                    }
                }
            }
        }

        ## Fill in total values
        for (j in 1:length(categories)) {
            for (k in 1:length(categories)) {
                ixy <- j + (length(categories) + 1) * length(categories)
                ixx <- k
                tab[ixy,ixx] <- sum(tab[seq(j, by=(length(categories) + 1), length.out=length(categories)), k], na.rm=TRUE)
            }
        }
        
        for (j in 1:(length(categories) + 1)) {
            for (k in 1:length(categories)) {
                ixy <- j * (length(categories) + 1)
                ixx <- k
                tab[ixy,ixx] <- sum(tab[(seq(1:length(categories)) + (j-1) * (length(categories) + 1)), k], na.rm=TRUE)
            }
        }

        for (j in 1:(length(categories) + 1)) {
            for (i in 1:(length(categories) + 1)) {
                ixy <- i + (j-1) * (length(categories) + 1)
                ixx <- length(categories) + 1
                tab[ixy,ixx] <- sum(tab[ixy,1:length(categories)], na.rm=TRUE)
            }
        }

        tables[[f]] <- tab
        
    }

    out <- new("ThreeMapComparison",
               tables=tables,
               factors=factors,
               categories=categories,
               labels=labels)

}
