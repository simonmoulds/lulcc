
#' @export
names.ExpVarMaps = function(x) x@varnames

#' @export
names.Performance = function(x) x@labels

#' @export
names.Prediction = function(x) x@labels

#' @export
names.PredModels = function(x) x@labels

## setMethod("names", "ExpVarMaps",
##           function(x) {
##               x@varnames
##           }
##           )

