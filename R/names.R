
#' @export
names.ExpVarRasterList = function(x) x@names

#' @export
names.PerformanceList = function(x) x@labels

#' @export
names.PredictionList = function(x) x@labels

#' @export
names.PredictiveModelList = function(x) x@labels

## setMethod("names", "ExpVarRasterList",
##           function(x) {
##               x@names
##           }
##           )

