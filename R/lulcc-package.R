#' @import methods raster
NULL

#' @importFrom stats binomial glm na.omit runif
NULL

#' lulcc: land use change modelling in R
#'
#' The lulcc package is an open and extensible framework for land use change
#' modelling in R.
#'
#' The aims of the package are as follows:
#'
#' \enumerate{
#'   \item to improve the reproducibility of scientific results and encourage
#'     reuse of code within the land use change modelling community
#'   \item to make it easy to directly compare and combine different model
#'     structures
#'   \item to allow users to perform several aspects of the modelling process
#'     within the same environment
#' }
#' 
#' To achieve these aims the package utilises an object-oriented approach based
#' on the S4 system, which provides a formal structure for the modelling
#' framework. Generic methods implemented for the \code{lulcc} classes include
#' \code{summary}, \code{show}, and \code{plot}.
#'
#' Land use change models are represented by objects inheriting from the
#' superclass \code{Model}. This class is designed to represent general
#' information required by all models while specific models are represented by
#' its subclasses. Currently the package includes two inductive land use change
#' models: the first is an implementation of the Change in Land Use and its
#' Effects at Small Regional extent (CLUE-S) model (Verburg et al., 2002) (class
#' \code{CluesModel}), while the second is an ordered procedure based on the
#' algorithm described by Fuchs et al. (2013) but modified to allow stochastic
#' transitions (class \code{OrderedModel}).
#'
#' The main input to inductive land use change models is a set of predictive
#' models relating observed land use or land use change to spatially explicit
#' explanatory variables. A predictive model is usually obtained for each
#' category or transition. In lulcc these models are represented by the class
#' \code{PredictiveModelList}. Currently lulcc supports binary logistic regression,
#' provided by base R (\code{glm}), recursive partitioning and regression trees,
#' provided by package \code{rpart} and random forest, provided by package
#' \code{randomForest}. To a large extent the success of the allocation routine
#' depends on the strength of the predictive models: this is one reason why an R
#' package for land use change modelling is attractive. 
#'
#' To validate model output lulcc includes a method developed by Pontius et al.
#' (2011) that simultaneously compares a reference map for time 1, a reference
#' map for time 2 and a simulated map for time 2 at multiple resolutions. In
#' lulcc the results of the comparison are represented by the class
#' \code{ThreeMapComparison}. From objects of this class it is straightforward
#' to extract information about different sources of agreement and disagreement,
#' represented by the class \code{AgreementBudget}, which can then be plotted. The
#' results of the comparison are conveniently summarised by the figure of merit,
#' represented by the class\code{FigureOfMerit}.
#'
#' In addition to the core functionality described above, lulcc inludes several
#' utility functions to assist with the model building process. Two example
#' datasets are also included. 
#'
#' @author Simon Moulds
#' @docType package
#' @name lulcc-package
#'
#' @references
#' Fuchs, R., Herold, M., Verburg, P.H., and Clevers, J.G.P.W. (2013). A
#' high-resolution and harmonized model approach for reconstructing and analysing
#' historic land changes in Europe, Biogeosciences, 10:1543-1559.
#' 
#' Pontius Jr, R.G., Peethambaram, S., Castella, J.C. (2011).
#' Comparison of three maps at multiple resol utions: a case study of land change
#' simulation in Cho Don District, Vietnam. Annals of the Association of American
#' Geographers 101(1): 45-62.
#'
#' Verburg, P.H., Soepboer, W., Veldkamp, A., Limpiada, R., Espaldon, V., Mastura,
#' S.S. (2002). Modeling the spatial dynamics of regional land use: the CLUE-S
#' model. Environmental management, 30(3):391-405.
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Plum Island Ecosystems
#' 
#' ## load data
#' data(pie)
#' 
#' ## observed maps
#' obs <- ObsLulcRasterStack(x=pie,
#'                           pattern="lu", 
#'                           categories=c(1,2,3), 
#'                           labels=c("Forest","Built","Other"), 
#'                           t=c(0,6,14))
#' obs
#' 
#' plot(obs)
#' 
#' crossTabulate(obs, times=c(0,14))
#' 
#' ## explanatory variables
#' ef <- ExpVarRasterList(x=pie, pattern="ef")
#' ef
#' 
#' part <- partition(x=obs[[1]], size=0.1, spatial=TRUE)
#' train.data <- getPredictiveModelInputData(obs=obs, ef=ef, cells=part[["train"]])
#' 
#' forms <- list(Built ~ ef_001+ef_002+ef_003,
#'               Forest ~ ef_001+ef_002,
#'               Other ~ ef_001+ef_002)
#' 
#' glm.models <- glmModels(formula=forms, family=binomial, data=train.data, obs=obs)
#' rpart.models <- rpartModels(formula=forms, data=train.data, obs=obs)
#' rf.models <- randomForestModels(formula=forms, data=train.data, obs=obs)
#' 
#' ## test ability of models to predict allocation of forest, built and other
#' ## land uses in testing partition
#' test.data <- getPredictiveModelInputData(obs=obs, ef=ef, cells=part[["test"]])
#' 
#' glm.pred <- PredictionList(models=glm.models, newdata=test.data)
#' glm.perf <- PerformanceList(pred=glm.pred, measure="rch")
#'
#' rpart.pred <- PredictionList(models=rpart.models, newdata=test.data)
#' rpart.perf <- PerformanceList(pred=rpart.pred, measure="rch")
#' 
#' rf.pred <- PredictionList(models=rf.models, newdata=test.data)
#' rf.perf <- PerformanceList(pred=rf.pred, measure="rch")
#' 
#' plot(list(glm=glm.perf, rpart=rpart.perf, rf=rf.perf))
#' 
#' ## test ability of models to predict location of urban gain 1985 to 1991
#' part <- rasterToPoints(obs[[1]], fun=function(x) x != 2, spatial=TRUE)
#' test.data <- getPredictiveModelInputData(obs=obs, ef=ef, cells=part, t=6)
#' 
#' glm.pred <- PredictionList(models=glm.models[[2]], newdata=test.data)
#' glm.perf <- PerformanceList(pred=glm.pred, measure="rch")
#' 
#' plot(list(glm=glm.perf))
#' 
#' ## obtain demand scenario
#' dmd <- approxExtrapDemand(obs=obs, tout=0:14)
#' matplot(dmd, type="l", ylab="Demand (no. of cells)", xlab="Time point",
#'         lty=1, col=c("Green","Red","Blue"))
#' legend("topleft", legend=obs@@labels, col=c("Green","Red","Blue"), lty=1)
#' 
#' ## get neighbourhood values
#' w <- matrix(data=1, nrow=3, ncol=3)
#' nb <- NeighbRasterStack(x=obs[[1]], weights=w, categories=2)
#' 
#' ## create CLUE-S model object
#' clues.rules <- matrix(data=1, nrow=3, ncol=3, byrow=TRUE) 
#' 
#' clues.parms <- list(jitter.f=0.0002,
#'                     scale.f=0.000001,
#'                     max.iter=1000,
#'                     max.diff=50, 
#'                     ave.diff=50) 
#' 
#' clues.model <- CluesModel(obs=obs,
#'                           ef=ef,
#'                           models=glm.models,
#'                           time=0:14,
#'                           demand=dmd,
#'                           elas=c(0.2,0.2,0.2),
#'                           rules=clues.rules,
#'                           params=clues.parms)
#' 
#' ## Create Ordered model
#' ordered.model <- OrderedModel(obs=obs,
#'                               ef=ef,
#'                               models=glm.models,
#'                               time=0:14,
#'                               demand=dmd,
#'                               order=c(2,1,3)) 
#' 
#' ## perform allocation
#' clues.model <- allocate(clues.model)
#' ordered.model <- allocate(ordered.model, stochastic=TRUE)
#' 
#' ## pattern validation
#' 
#' ## CLUE-S
#' clues.tabs <- ThreeMapComparison(x=clues.model,
#'                                  factors=2^(1:8),
#'                                  timestep=14)
#' plot(clues.tabs)
#' plot(clues.tabs, category=1, factors=2^(1:8)[c(1,3,5,7)])
#' 
#' ## Ordered
#' ordered.tabs <- ThreeMapComparison(x=ordered.model,
#'                                  factors=2^(1:8),
#'                                  timestep=14)
#' plot(ordered.tabs)
#' plot(ordered.tabs, category=1, factors=2^(1:8)[c(1,3,5,7)])
#' 
#' ## calculate agreement budget and plot
#' 
#' ## CLUE-S
#' clues.agr <- AgreementBudget(x=clues.tabs)
#' plot(clues.agr, from=1, to=2)
#' 
#' ## Ordered
#' ordered.agr <- AgreementBudget(x=ordered.tabs)
#' plot(ordered.agr, from=1, to=2)
#' 
#' ## calculate Figure of Merit and plot
#' 
#' ## CLUE-S
#' clues.fom <- FigureOfMerit(x=clues.tabs)
#' p1 <- plot(clues.fom, from=1, to=2)
#' 
#' ## Ordered
#' ordered.fom <- FigureOfMerit(x=ordered.tabs)
#' p2 <- plot(ordered.fom, from=1, to=2)
#'
#' }
#'
NULL
