#' @include class-ModelInput.R
NULL

## if (!isGeneric("allocate")) {
##     setGeneric("allocate", function(input, ...)
##                standardGeneric("allocate"))
## }

#' @title Allocate land use change spatially
#'
#' Perform spatially explicit allocation of land use change using different
#' methods. Currently the function provides an algorithm based on the CLUE-S
#' model (Verburg et al., 2002) and a novel stochastic procedure. 
#'
#' \code{allocate} is designed to be easily extensible. To write a new allocation
#' method (apart from writing the algorithm itself) one simply needs to create a
#' new class inheriting from \code{ModelInput} and an \code{allocate} method for
#' this class.
#'
#' @param input an object inheriting from class \code{ModelInput}
#' @param ... additional arguments for specific methods
#'
#' @seealso \code{link{ModelInput}}
#' @author Simon Moulds
#'
#' @export
#' @rdname allocate
#'
#' @references Verburg, P.H., Soepboer, W., Veldkamp, A., Limpiada, R., Espaldon,
#' V., Mastura, S.S. (2002). Modeling the spatial dynamics of regional land use:
#' the CLUE-S model. Environmental management, 30(3):391-405.

setGeneric("allocate", function(input, ...)
           standardGeneric("allocate"))

#' @rdname allocate
#' @aliases allocate,CluesModelInput-method
setMethod("allocate", signature(input = "CluesModelInput"),
          function(input, ...) {
              maps <- .allocate(input=input, fun=.clues)
          }
)

#' @rdname allocate
#' @aliases allocate,OrderedModelInput-method
setMethod("allocate", signature(input = "OrderedModelInput"),
          function(input, ...) {
              maps <- .allocate(input=input, fun=.ordered)
          }
)

.allocate <- function(input, fun, ...) {              

    cells <- which(!is.na(raster::getValues(input@map0)))
    map0.vals <- raster::extract(input@map0, cells)
    hist.vals <- raster::extract(input@hist, cells)
    mask.vals <- raster::extract(input@mask, cells)
    newdata <- as.data.frame(x=input@pred, cells=cells)
    prob <- calcProb(object=input@models, newdata=newdata)
    maps <- raster::stack(input@map0)
              
    for (i in 1:(nrow(input@demand) - 1)) {
         print(i)                                    
         d <- input@demand[(i+1),] ## demand for current timestep
         if (input@pred@dynamic && i > 1) {
             newdata <- .update.data.frame(x=newdata, y=input@pred, map=input@map0, cells=cells, timestep=(i-1))
             prob <- calcProb(object=input@models, newdata=newdata)
         }
         tprob <- prob

         ## elas only included in some models, so check whether model input has slot
         if (.hasSlot(input, "elas")) { 
             for (j in 1:length(input@categories)) {
                 ix <- map0.vals %in% input@categories[j]
                 tprob[ix,j] <- tprob[ix,j] + input@elas[j] ## add elasticity
             }
         }
                  
         if (!is.null(input@neighb)) {
             nb.allow <- allowNeighb(x=input@neighb, cells=cells, categories=input@categories, rules=input@nb.rules)
             tprob <- tprob * nb.allow ## neighbourhood decision rules
         }
                  
         ## implement other decision rules
         if (!is.null(input@rules)) {
             cd <- d - input@demand[i,] ## change direction
             allow <- allow(x=map0.vals, hist=hist.vals, categories=input@categories, cd=cd,rules=input@rules)
             tprob <- tprob * allow
         }

         ## make automatic conversions if necessary
         auto <- .autoConvert(x=map0.vals, mask=mask.vals, prob=tprob, categories=input@categories)
         map0.vals[auto$ix] <- auto$vals
         tprob[auto$ix,] <- NA
                  
         ## allocation
         args <- c(list(tprob=tprob, map0.vals=map0.vals, demand=d, categories=input@categories), input@params)
         map1.vals <- do.call(fun, args)
         map1 <- raster::raster(input@map0, ...) 
         map1[cells] <- map1.vals
         maps <- raster::stack(maps, map1)
    
         ## prepare input for next timestep
         if (i < nrow(input@demand)) {
             hist.vals <- .updatehist(map0.vals, map1.vals, hist.vals) ## update
             map0.vals <- map1.vals 
             if (!is.null(input@neighb)) input@neighb <- NeighbMaps(x=map1, neighb=input@neighb)
         }
     }    
     out <- maps              
}

#' @useDynLib lulccR
.clues <- function(tprob, map0.vals, demand, categories, jitter.f, scale.f, max.iter, max.diff, ave.diff) {
    map1.vals <- .Call("allocateclues", tprob, map0.vals, demand, categories, jitter.f, scale.f, max.iter, max.diff, ave.diff)
}

#' @useDynLib lulccR
.ordered <- function(tprob, map0.vals, demand, categories, max.diff) {

    map0.area <- .Call("total", map0.vals, categories) ## initial condition
    diff <- demand - map0.area
    if (sum(abs(diff)) < max.diff) return(map0.vals)

    map1.vals <- map0.vals
    map1.area <- map0.area
    
    repeat {
        
        tmp.demand <- demand - map1.area ## number of cells to change
        decr <- list(ix=which(tmp.demand < 0), lu=categories[which(tmp.demand < 0)]) ## lu types with decreasing demand  
        incr <- list(ix=which(tmp.demand > 0), lu=categories[which(tmp.demand > 0)]) ## lu types with increasing demand
        tmp.demand <- abs(tmp.demand)
       
        tprob.max <- apply(as.data.frame(tprob[, incr$ix]), 1, .maxtprob)
        tprob.max.ix <- order(tprob.max, na.last=NA, decreasing=TRUE)
        tprob.max.x <- tprob.max[tprob.max.ix]
        
        ## find index of cells belonging to classes that must decrease area
        decr.lu.ix <- which(map1.vals %in% decr$lu) 
        tprob.max.x <- tprob.max.x[tprob.max.ix %in% decr.lu.ix]  
        tprob.max.ix <- tprob.max.ix[tprob.max.ix %in% decr.lu.ix] ## does this preserve order?
        decr.lu.vals <- map1.vals[tprob.max.ix]
        
        rand.unif <- runif(length(tprob.max.ix))

        ## ## rescale tprob.max.x
        ## tprob.range <- range(tprob.max.x, na.rm=TRUE)
        ## tprob.max.x <- ((tprob.max.x - tprob.range[1]) / diff(tprob.range))

        i <- 0
        repeat {
            i <- i+1
            ix <- tprob.max.ix[i]
            maxt <- tprob.max.x[i]

            if (maxt > rand.unif[i]) {
                lu0 <- incr$lu[which.max(tprob[ix , incr$ix])]
                lu0.ix <- incr$ix[which(incr$lu == lu0)]
                lu1 <- decr.lu.vals[i]
                lu1.ix <- decr$ix[which(decr$lu == lu1)]
                map1.vals[ix] <- lu0 ## allocate change

                tmp.demand[lu0.ix] <- tmp.demand[lu0.ix] - 1
                tmp.demand[lu1.ix] <- tmp.demand[lu1.ix] - 1

                if (tmp.demand[lu0.ix] == 0 | tmp.demand[lu1.ix] == 0) break
                
            }

            if (i >= length(tprob.max.ix)) break
            
        }

        map1.area <- .Call("total", map1.vals, categories)
        diff <- demand - map1.area
            
        ## condition to break outer repeat loop
        if (sum(abs(diff)) < max.diff) {
            break
        }
        
    }
    
    map1.vals

}

## helper functions

#' @useDynLib lulccR
.updatehist <- function(lu0, lu1, hist) {
    hist <- .Call("updatehist", lu0, lu1, hist)
}

.maxtprob <- function(x) {    
    if (length(which(!is.na(x)) > 0)) {
        out <- max(x, na.rm=TRUE)
    } else {
        out <- NA
    }
}

#' @useDynLib lulccR
.autoConvert <- function(x, mask, prob, categories, ...) {
    if (length(x) != length(mask)) stop("mask does not correspond with x")
    vals <- .Call("autoconvert", x, mask, prob, categories)
    ix <- which(!is.na(vals))
    vals <- vals[ix]
    out <- list(ix=ix, vals=vals)
}



