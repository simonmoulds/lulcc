#' @include index.R

#' @export
setAs("ObsLulcRasterStack","RasterStack",
      function(from) {
          maps <- list()
          nl <- nlayers(from)
          for (i in 1:nl) {
              maps[[i]] <- from[[i]]
          }
          stack(maps)
      }
      )
