#' @include class-PredModels.R
NULL

#' Create a PredModels object
#'
#' Methods to create a \code{\link{PredModels-class}} object.
#'
#' @param models a list of predictive models corresponding to
#'   \code{obs@@categories} or \code{categories}
#' @param obs an ObsLulcMaps object
#' @param categories numeric vector of land use categories in observed maps. Only
#'   required if \code{obs} is not provided
#' @param labels character vector (optional) with labels corresponding to
#'   \code{categories}. Only required if \code{obs} is not provided
#' @param \dots additional arguments (none)
#'
#' @seealso \code{\link[stats]{glm}}, \code{rpart::\link[rpart]{rpart}},
#' \code{randomForest::\link[randomForest]{randomForest}}
#'
#' @return A \code{\link{PredModels}} object.
#'
#' @export
#' @rdname PredModels
#'
#' @examples
#' 
#' ## Plum Island Ecosystems
#'
#' ## Load observed land use maps
#' obs <- ObsLulcMaps(x=pie,
#'                    pattern="lu",
#'                    categories=c(1,2,3),
#'                    labels=c("forest","built","other"),
#'                    t=c(0,6,14))
#'
#' ## Load explanatory variables
#' ef <- ExpVarMaps(x=pie, pattern="ef")
#'
#' ## create equally sized training and testing partitions
#' part <- partition(x=obs@@maps[[1]], size=0.5, spatial=FALSE)
#' 
#' ## convert initial land use map to RasterBrick where each layer is a boolean
#' ## map for the respective land use
#' br <- raster::layerize(obs@@maps[[1]])
#' names(br) <- obs@@labels
#'
#' ## create data.frame to fit models
#' train.df <- raster::extract(x=br, y=part$train, df=TRUE)
#' train.df <- cbind(train.df, as.data.frame(x=ef, cells=part$train))
#'
#' ## model formulas
#' forest.formula <- formula(forest ~ 1)
#' built.formula <- formula(built~ef_001+ef_002+ef_003)
#' other.formula <- formula(built~ef_001+ef_002)
#'
#' ## glm models
#' forest.glm <- glm(formula=forest.formula, family=binomial, data=train.df)
#' built.glm <- glm(formula=built.formula, family=binomial, data=train.df)
#' other.glm <- glm(formula=other.formula, family=binomial, data=train.df)
#'
#' ## NB rpart and randomForest do not accept null model formula, so only fit
#' ## models for 'built' and 'other' classes
#'
#' ## rpart models
#' built.rp <- rpart::rpart(formula=built.formula, data=train.df, method="class")
#' other.rp <- rpart::rpart(formula=other.formula, data=train.df, method="class")
#'
#' ## random forest models (warning: takes a long time!)
#' built.rf <- randomForest::randomForest(formula=built.formula, data=train.df)
#' other.rf <- randomForest::randomForest(formula=other.formula, data=train.df)
#'
#' ## create PredModels object
#' glm.models <- PredModels(models=list(forest.glm, built.glm, other.glm),
#'                          obs=obs)
#'
#' rp.models <- PredModels(models=list(built.rp, other.rp),
#'                         categories=obs@@categories[2:3],
#'                         labels=obs@@labels[2:3])
#'
#' rf.models <- PredModels(model=list(built.rf, other.rf),
#'                         categories=obs@@categories[2:3],
#'                         labels=obs@@labels[2:3])
#'
#' ## obtain Prediction objects
#' glm.pred <- Prediction(models=glm.models, obs=obs, ef=ef, partition=part$test)
#' rp.pred <- Prediction(models=rp.models, obs=obs, ef=ef, partition=part$test)
#' rf.pred <- Prediction(models=rf.models, obs=obs, ef=ef, partition=part$test)
#'
#' ## quickly compare area under the curve
#' compareAUC(pred=list(glm=glm.pred, rpart=rp.pred, randomForest=rf.pred))
#'
#' ## obtain Performance objects
#' glm.perf <- Performance(pred=glm.pred, measure="rch")
#' rp.perf <- Performance(pred=rp.pred, measure="rch")
#' rf.perf <- Performance(pred=rf.pred, measure="rch")
#' 
#' ## plot ROC curve
#' p <- Performance.plot(list(glm=glm.perf, rpart=rp.perf, rf=rf.perf),
#'                       layout=c(3,1),
#'                       aspect="iso",
#'                       xlab=list(label=""),
#'                       ylab=list(label=""),
#'                       scales=list(cex=0.6),
#'                       key.args=list(cex=0.4, size=2.5),
#'                       par.strip.text=list(cex=0.6),
#'                       par.settings=list(strip.background=
#'                                           list(col="lightgrey")))
#'
#' ## view plot
#' print(p)

setGeneric("PredModels", function(models, obs, categories, labels, ...)
           standardGeneric("PredModels"))

#' @rdname PredModels
#' @aliases PredModels,list,ObsLulcMaps,ANY,ANY-method
setMethod("PredModels", signature(models = "list", obs = "ObsLulcMaps", categories = "ANY", labels = "ANY"),
          function(models, obs, categories, labels, ...) {
              if (length(models) != length(obs@labels)) stop("")
              ## types <- predModelType(x=models)
              ## new("PredModels", models=models, types=types, categories=obs@categories, labels=obs@labels)
              new("PredModels", models=models, categories=obs@categories, labels=obs@labels)
          }
)

#' @rdname PredModels
#' @aliases PredModels,list,ANY,ANY,numeric,character-method
setMethod("PredModels", signature(models = "list", obs = "ANY", categories = "numeric", labels = "character"),
          function(models, obs, categories, labels, ...) {
              if (length(models) != length(labels)) stop("")
              if (length(models) != length(categories)) stop("")
              ## types <- predModelType(x=models)
              ## new("PredModels", models=models, types=types, categories=categories, labels=labels)
              new("PredModels", models=models, categories=categories, labels=labels)
          }
)
