#' @include index.R

#' @export
setAs("ObsLulcMaps","RasterStack",
      function(from) {
          maps <- list()
          nl <- nlayers(from)
          for (i in 1:nl) {
              maps[[i]] <- from[[i]]
          }
          stack(maps)
      }
      )

#' @export
setAs("Model","ModelInput",
      function(from) {
          new("ModelInput",
              obs=from@obs,
              ef=from@ef,
              time=from@time,
              demand=from@demand)
      }
      )
