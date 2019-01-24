
#' @export
length.PerformanceList = function(x) length(x@performance)

#' @export
length.PredictionList = function(x) length(x@prediction)

#' @export
length.ExpVarRasterList = function(x) length(x@maps)

#' @export
length.PredictiveModelList = function(x) length(x@models)
