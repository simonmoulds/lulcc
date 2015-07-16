#' @include class-ObsLulcMaps.R class-ExpVarMaps.R
NULL

#' Show
#'
#' Show objects
#'
#' @param object TODO
#'
#' @return TODO
#' 
#' @export
#' @rdname show-methods
#'
#' @examples
#'
#' ## TODO

#' @rdname show-methods
#' @aliases show,ExpVarMaps-method
setMethod("show", "ExpVarMaps",
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
#' @aliases show,PredModels-method
setMethod("show", "PredModels",
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
#' @aliases show,Prediction-method
setMethod("show", "Prediction",
          function(object) {
              cat("class       : ", class(object), "\n", sep="")
              cat("length      : ", length(object), "\n", sep="")
              cat("names       : ", paste0(names(object), collapse=(", ")), "\n", sep="")
          }
          )
                  
#' @rdname show-methods
#' @aliases show,Performance-method
setMethod("show", "Performance",
          function(object) {
              cat("\nclass       : ", class(object), "\n", sep="")
              cat("length      : ", length(object), "\n", sep="")
              cat("names       : ", paste0(names(object), collapse=(", ")), "\n", sep="")
          }
          )
                  
#' @rdname show-methods
#' @aliases show,ModelInput-method
setMethod("show", "ModelInput",
          function(object) {
              cat("\nclass                : ", class(object), "\n", sep="")

              cat("\n-------------------------------------------\n")
              cat("Input data:\n\n")

              cat("initial observed map : ", names(object@obs)[1], "\n", sep="")
              cat("explanatory factors  : ", paste0(names(object@ef), collapse=", "), "\n", sep="")
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

              dmd0 <- c(object@demand[1,], sum(object@demand[1,]))
              dmd1 <- c(object@demand[nrow(object@demand),], sum(object@demand[nrow(object@demand),]))
              
              if (nl > mnr) {
                  dmd0 <- c(dmd0[1:mnr], "...")
                  dmd1 <- c(dmd1[1:mnr], "...")

              }

              w <- pmax(nchar(ln), nchar(dmd0), nchar(dmd1))
              m <- rbind(ln, dmd0, dmd1)

              ## a loop because 'width' is not recycled by format
              for (i in 1:ncol(m)) {
                  m[,i]   <- format(m[,i], width=w[i], justify="right")

              }

              t1 <- formatC(object@time[length(object@time)], width=-3)
              cat("land use classes     : ", paste(m[1,], collapse="  "), "\n", sep="")
              cat("demand at t=0        : ", paste(m[2,], collapse="  "), "\n", sep="")
              cat("demand at t=", t1, "      : ", paste(m[3,], collapse="  "), "\n", sep="")
              cat("\n-------------------------------------------\n")
              cat("Model region (defined by ObsLulcMaps object):\n\n")    
              cat('dimensions           : ', nrow(object@obs), ', ', ncol(object@obs), ', ', ncell(object@obs),'  (nrow, ncol, ncell)\n', sep="" )
              cat('resolution           : ' , xres(object@obs), ', ', yres(object@obs), '  (x, y)\n', sep="")
              cat('extent               : ' , object@obs@extent@xmin, ', ', object@obs@extent@xmax, ', ', object@obs@extent@ymin, ', ', object@obs@extent@ymax, '  (xmin, xmax, ymin, ymax)\n', sep="")
              cat('coord. ref.          :' , projection(object@obs, TRUE), '\n\n')
              
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

              dmd0 <- c(object@demand[1,], sum(object@demand[1,]))
              dmd1 <- c(object@demand[nrow(object@demand),], sum(object@demand[nrow(object@demand),]))
              
              if (nl > mnr) {
                  dmd0 <- c(dmd0[1:mnr], "...")
                  dmd1 <- c(dmd1[1:mnr], "...")

              }

              w <- pmax(nchar(ln), nchar(dmd0), nchar(dmd1))
              m <- rbind(ln, dmd0, dmd1)

              ## a loop because 'width' is not recycled by format
              for (i in 1:ncol(m)) {
                  m[,i]   <- format(m[,i], width=w[i], justify="right")

              }

              t1 <- formatC(object@time[length(object@time)], width=-3)
              cat("land use classes     : ", paste(m[1,], collapse="  "), "\n", sep="")
              cat("demand at t=0        : ", paste(m[2,], collapse="  "), "\n", sep="")
              cat("demand at t=", t1, "      : ", paste(m[3,], collapse="  "), "\n", sep="")
              out <- "No"
              if (is(object@output, "RasterStack")) out <- "Yes"
              cat("contains output?     : ", out, "\n", sep="")
              
              cat("\n-------------------------------------------\n")
              cat("Model region (defined by ObsLulcMaps object):\n\n")    
              cat('dimensions           : ', nrow(object@obs), ', ', ncol(object@obs), ', ', ncell(object@obs),'  (nrow, ncol, ncell)\n', sep="" )
              cat('resolution           : ' , xres(object@obs), ', ', yres(object@obs), '  (x, y)\n', sep="")
              cat('extent               : ' , object@obs@extent@xmin, ', ', object@obs@extent@xmax, ', ', object@obs@extent@ymin, ', ', object@obs@extent@ymax, '  (xmin, xmax, ymin, ymax)\n', sep="")
              cat('coord. ref.          :' , projection(object@obs, TRUE), '\n\n')
              
          }
          )

#' @rdname show-methods
#' @aliases show,CluesModel-method
setMethod("show", "CluesModel",
          function(object) {
              cat("\nclass                : ", class(object), "\n", sep="")

              cat("\n-------------------------------------------\n")
              cat("Input data:\n\n")

              cat("initial observed map : ", names(object@obs)[1], "\n", sep="")
              cat("explanatory factors  : ", paste0(names(object@ef), collapse=", "), "\n", sep="")
              cat("mask file            : ", names(object@mask), "\n", sep="")
              cat("history file         : ", names(object@hist), "\n", sep="")
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

              type <- sapply(object@models@models, FUN=function(x) class(x)[1])
              neighb <- rep("No", length(type))
              
              if (!is.null(object@neighb)) {
                  ix <- object@categories %in% object@neighb@categories
                  neighb[ix] <- "Yes"
              }

              dmd0 <- c(object@demand[1,], sum(object@demand[1,]))
              dmd1 <- c(object@demand[nrow(object@demand),], sum(object@demand[nrow(object@demand),]))
              type <- c(type, NA)
              neighb <- c(neighb, NA)
              
              if (nl > mnr) {
                  dmd0 <- c(dmd0[1:mnr], "...")
                  dmd1 <- c(dmd1[1:mnr], "...")
                  type <- c(type[1:mnr], "...")
                  neighb <- c(neighb[1:mnr], "...")

              }

              w <- pmax(nchar(ln), nchar(dmd0), nchar(dmd1), nchar(type), nchar(neighb))
              m <- rbind(ln, type, neighb, dmd0, dmd1)

              ## a loop because 'width' is not recycled by format
              for (i in 1:ncol(m)) {
                  m[,i]   <- format(m[,i], width=w[i], justify="right")

              }

              t1 <- formatC(object@time[length(object@time)], width=-3)
              cat("land use classes     : ", paste(m[1,], collapse="  "), "\n", sep="")
              cat("model type           : ", paste(m[2,], collapse="  "), "\n", sep="")
              cat("neighbourhood        : ", paste(m[3,], collapse="  "), "\n", sep="")
              
              cat("demand at t=0        : ", paste(m[4,], collapse="  "), "\n", sep="")
              cat("demand at t=", t1, "      : ", paste(m[5,], collapse="  "), "\n", sep="")
              out <- "No"
              if (is(object@output, "RasterStack")) out <- "Yes"
              cat("contains output?     : ", out, "\n", sep="")
              
              cat("\n-------------------------------------------\n")
              cat("Model region (defined by ObsLulcMaps object):\n\n")    
              cat('dimensions           : ', nrow(object@obs), ', ', ncol(object@obs), ', ', ncell(object@obs),'  (nrow, ncol, ncell)\n', sep="" )
              cat('resolution           : ' , xres(object@obs), ', ', yres(object@obs), '  (x, y)\n', sep="")
              cat('extent               : ' , object@obs@extent@xmin, ', ', object@obs@extent@xmax, ', ', object@obs@extent@ymin, ', ', object@obs@extent@ymax, '  (xmin, xmax, ymin, ymax)\n', sep="")
              cat('coord. ref.          :' , projection(object@obs, TRUE), '\n\n')

          }
          )

#' @rdname show-methods
#' @aliases show,OrderedModel-method
setMethod("show", "OrderedModel",
          function(object) {
              cat("\nclass                : ", class(object), "\n", sep="")

              cat("\n-------------------------------------------\n")
              cat("Input data:\n\n")

              cat("initial observed map : ", names(object@obs)[1], "\n", sep="")
              cat("explanatory factors  : ", paste0(names(object@ef), collapse=", "), "\n", sep="")
              cat("mask file            : ", names(object@mask), "\n", sep="")
              cat("history file         : ", names(object@hist), "\n", sep="")
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

              type <- sapply(object@models@models, FUN=function(x) class(x)[1])
              neighb <- rep("No", length(type))
              order <- object@order
              
              if (!is.null(object@neighb)) {
                  ix <- object@categories %in% object@neighb@categories
                  neighb[ix] <- "Yes"
              }

              dmd0 <- c(object@demand[1,], sum(object@demand[1,]))
              dmd1 <- c(object@demand[nrow(object@demand),], sum(object@demand[nrow(object@demand),]))

              type <- c(type, NA)
              neighb <- c(neighb, NA)
              order <- c(order, NA)
              
              if (nl > mnr) {
                  dmd0 <- c(dmd0[1:mnr], "...")
                  dmd1 <- c(dmd1[1:mnr], "...")
                  type <- c(type[1:mnr], "...")
                  neighb <- c(neighb[1:mnr], "...")
                  order <- c(order[1:mnr], "...")
              }

              w <- pmax(nchar(ln), nchar(dmd0), nchar(dmd1), nchar(type), nchar(neighb), nchar(order))
              m <- rbind(ln, order, type, neighb, dmd0, dmd1)

              ## a loop because 'width' is not recycled by format
              for (i in 1:ncol(m)) {
                  m[,i]   <- format(m[,i], width=w[i], justify="right")

              }

              t1 <- formatC(object@time[length(object@time)], width=-3)
              cat("land use classes     : ", paste(m[1,], collapse="  "), "\n", sep="")
              cat("allocation order     : ", paste(m[2,], collapse="  "), "\n", sep="")
              cat("model type           : ", paste(m[3,], collapse="  "), "\n", sep="")
              cat("neighbourhood        : ", paste(m[4,], collapse="  "), "\n", sep="")
              
              cat("demand at t=0        : ", paste(m[5,], collapse="  "), "\n", sep="")
              cat("demand at t=", t1, "      : ", paste(m[6,], collapse="  "), "\n", sep="")
              out <- "No"
              if (is(object@output, "RasterStack")) out <- "Yes"
              cat("contains output?     : ", out, "\n", sep="")
              
              cat("\n-------------------------------------------\n")
              cat("Model region (defined by ObsLulcMaps object):\n\n")    
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
              cat("allocation model     : ", "\n", sep="")
              cat("reference t0         : ", "\n", sep="")
              cat("reference t1         : ", "\n", sep="")
              cat("simulated t1         : ", "\n", sep="")
              cat("factors              : ", paste0(object@factors, collapse=", "), "\n", sep="")
              
          }
          )

