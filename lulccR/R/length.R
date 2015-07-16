
#' @export
length.Performance = function(x) length(x@performance)

#' @export
length.Prediction = function(x) length(x@prediction)

#' @export
length.ExpVarMaps = function(x) length(x@maps)

#' @export
length.PredModels = function(x) length(x@models)
