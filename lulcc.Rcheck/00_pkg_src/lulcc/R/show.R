#' @include class-ObsLulcRasterStack.R class-ExpVarRasterList.R
NULL

#' Show
#'
#' Show objects
#'
#' @param object an object belonging to one of the classes in \code{lulcc}
#' 
#' @export
#' @rdname show-methods

#' @rdname show-methods
#' @aliases show,ExpVarRasterList-method
setMethod("show", "ExpVarRasterList",
          function(object) {
              for (i in 1:length(object)) {
                  cat("-------------------------\n\n")
                  cat(i,". ", names(object)[i], "\n\n", sep="")
                  show(object@maps[[i]])
              }
              cat("-------------------------\n\n")
          }   
          )

#' @rdname show-methods
#' @aliases show,PredictiveModelList-method
setMethod("show", "PredictiveModelList",
          function(object) {
              cat("class       : ", class(object), "\n", sep="")
              cat("length      : ", length(object), "\n", sep="")
              cat("names       : ", paste0(names(object), collapse=(", ")), "\n", sep="")
              cat("\n")
              
              for (i in 1:length(object)) {
                  cat("-------------------------\n\n")
                  cat("Model for land use class ", paste0(names(object)[i], " (", object@categories[i], "):"), "\n", sep="")
                  show(object@models[[i]])
                  cat("\n")
              }              

              cat("-------------------------\n\n")
          }
          )

#' @rdname show-methods
#' @aliases show,PredictionList-method
setMethod("show", "PredictionList",
          function(object) {
              cat("class       : ", class(object), "\n", sep="")
              cat("length      : ", length(object), "\n", sep="")
              cat("names       : ", paste0(names(object), collapse=(", ")), "\n", sep="")
          }
          )
                  
#' @rdname show-methods
#' @aliases show,PerformanceList-method
setMethod("show", "PerformanceList",
          function(object) {
              cat("\nclass       : ", class(object), "\n", sep="")
              cat("length      : ", length(object), "\n", sep="")
              cat("names       : ", paste0(names(object), collapse=(", ")), "\n", sep="")
          }
          )
                  
#' @rdname show-methods
#' @aliases show,Model-method
setMethod("show", "Model",
          function(object) {
              cat("\nclass                : ", class(object), "\n", sep="")

              cat("\n-------------------------------------------\n")
              cat("Input data:\n\n")

              cat("initial observed map : ", names(object@obs)[1], "\n", sep="")
              cat("explanatory factors  : ", paste0(names(object@ef), collapse=", "), "\n", sep="")
              if (.hasSlot(object, "mask")) cat("mask file            : ", names(object@mask), "\n", sep="")
              if (.hasSlot(object, "hist")) cat("history file         : ", names(object@hist), "\n", sep="")
              
              cat("no. time points      : ", length(object@time), "\n", sep="")

              mnr <- 15		
              nl <- length(object@obs@labels)
              ln <- c(object@obs@labels, "Total")
	      if (nl > mnr) {
                  ln <- c(ln[1:mnr], '...')
	      }

              n <- nchar(ln)
              if (nl > 5) {
                  b <- n > 26
                  if (any(b)) {
                      ln[b] <- paste(substr(ln[b], 1, 9), '//', substr(ln[b], nchar(ln[b])-9, nchar(ln[b])), sep='')
                  }
              }

              dmd0 <- NULL
              dmd1 <- NULL
              type <- NULL
              neighb <- NULL
              order <- NULL
              if (.hasSlot(object, "models")) type <- sapply(object@models@models, FUN=function(x) class(x)[1])

              if (.hasSlot(object, "neighb")) {
                  neighb <- rep("No", length(type))

                  if (!is.null(object@neighb)) {
                      ix <- object@categories %in% object@neighb@categories
                      neighb[ix] <- "Yes"
                  }
              }

              if (.hasSlot(object, "order")) order <- object@order

              if (.hasSlot(object, "demand")) {
                  dmd0 <- c(object@demand[1,], sum(object@demand[1,]))
                  dmd1 <- c(object@demand[nrow(object@demand),], sum(object@demand[nrow(object@demand),]))
              }
              
              if (.hasSlot(object, "models")) type <- c(type, NA)
              if (.hasSlot(object, "neighb")) neighb <- c(neighb, NA)
              if (.hasSlot(object, "order"))  order <- c(order, NA)
              
              if (nl > mnr) {
                  if (.hasSlot(object, "demand")) {
                      dmd0 <- c(dmd0[1:mnr], "...")
                      dmd1 <- c(dmd1[1:mnr], "...")
                  }
                  
                  if (.hasSlot(object, "models")) type <- c(type[1:mnr], "...")
                  if (.hasSlot(object, "neighb")) neighb <- c(neighb[1:mnr], "...")
                  if (.hasSlot(object, "order"))  order <- c(order[1:mnr], "...")
              }

              w <- pmax(nchar(ln))
              if (.hasSlot(object, "demand")) w <- pmax(w, nchar(dmd0), nchar(dmd1))

              if (is.null(type))   type <- rep(NA, length(ln))
              if (is.null(neighb)) neighb <- rep(NA, length(ln))
              if (is.null(order))  order <- rep(NA, length(ln))
              
              m <- rbind(ln, dmd0, dmd1, order, type, neighb)

              ## a loop because 'width' is not recycled by format
              for (i in 1:ncol(m)) {
                  m[,i]   <- format(m[,i], width=w[i], justify="right")
              }

              t1 <- formatC(object@time[length(object@time)], width=-3)
              cat("land use classes     : ", paste(m[1,], collapse="  "), "\n", sep="")
              if (.hasSlot(object, "order"))  cat("allocation order     : ", paste(m[4,], collapse="  "), "\n", sep="")
              if (.hasSlot(object, "models")) cat("model type           : ", paste(m[5,], collapse="  "), "\n", sep="")
              if (.hasSlot(object, "neighb")) cat("neighbourhood        : ", paste(m[6,], collapse="  "), "\n", sep="")
              if (.hasSlot(object, "demand")) {
                  cat("demand at t=0        : ", paste(m[2,], collapse="  "), "\n", sep="")
                  cat("demand at t=", t1, "      : ", paste(m[3,], collapse="  "), "\n", sep="")
              }

              if (.hasSlot(object, "output")) {
                  out <- "No"
                  if (is(object@output, "RasterStack")) out <- "Yes"
                  cat("contains output?     : ", out, "\n", sep="")
              }

              cat("\n-------------------------------------------\n")
              cat("Model region (defined by ObsLulcRasterStack object):\n\n")    
              cat('dimensions           : ', nrow(object@obs), ', ', ncol(object@obs), ', ', ncell(object@obs),'  (nrow, ncol, ncell)\n', sep="" )
              cat('resolution           : ' , xres(object@obs), ', ', yres(object@obs), '  (x, y)\n', sep="")
              cat('extent               : ' , object@obs@extent@xmin, ', ', object@obs@extent@xmax, ', ', object@obs@extent@ymin, ', ', object@obs@extent@ymax, '  (xmin, xmax, ymin, ymax)\n', sep="")
              cat('coord. ref.          :' , projection(object@obs, TRUE), '\n\n')
              
          }
          )

#' @rdname show-methods
#' @aliases show,ThreeMapComparison-method
setMethod("show", "ThreeMapComparison",
          function(object) {
              cat("class                : ", class(object), "\n", sep="")
              cat("factors              : ", paste0(object@factors, collapse=", "), "\n", sep="")              
          }
          )

