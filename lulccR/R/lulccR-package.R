#' @import methods raster
NULL

#' lulccR: land use change modelling in R
#'
#' The lulccR package is an open and extensible framework for land use change
#' modelling in R.
#'
#' The aims of the package are as follows:
#'
#' \enumerate{
#'   \item to improve the reproducibility of scientific results and encourage
#'     reuse of code within the land use change modelling community
#'   \item to make it easy to directly compare and combine different model
#'     structures
#'   \item to allow users to perform every aspect of the modelling process within the same
#'     environment
#' }
#' 
#' To achieve these aims the package utilises an object-oriented approach based
#' on the S4 system, which provides a formal structure for the modelling
#' framework.
#'
#' Models are represented by objects inheriting from the superclass \code{Model}.
#' This class is designed to represent general information required by all
#' models while specific models are represented by its subclasses. Currently the
#' package includes two inductive land use change models: the first is an
#' implementation of the Change in Land Use and its Effects at Small Regional
#' extent (CLUE-S) model (Verburg et al., 2002) (class \code{CluesModel}), while
#' the second is an ordered procedure based on the algorithm described by
#' Fuchs et al. (2013) but modified to allow stochastic transitions (class
#' \code{OrderedModel}).
#'
#' The main input to land use change models is a set of predictive models
#' relating observed land use or land use change to spatially explicit
#' explanatory variables. A predictive model is usually obtained for each
#' category or transition. In lulccR these models are represented by the class
#' \code{PredModels}. Currently lulccR supports binary logistic regression,
#' provided by base R (\code{glm}), recursive partitioning and regression trees,
#' provided by package \code{rpart} and random forest, provided by package
#' \code{randomForest}. To a large extent, the success of the allocation routine
#' depends on the strength of the predictive models: this is one reason why an R
#' package for land use change modelling is attractive. 
#'
#' To validate model output lulccR includes a method developed by Pontius et al.
#' (2011) that simultaneously compares a reference map for time 1, a reference
#' map for time 2 and a simulated map for time 2 at multiple resolutions. In
#' lulccR the results of the comparison are represented by the class
#' \code{ThreeMapComparison}. From objects of this class it is straightforward
#' to extract information about different sources of agreement and disagreement,
#' represented by the class \code{AgreementBudget}, which can then be plotted. The
#' results of the comparison are conveniently summarised by the figure of merit,
#' represented by the class\code{FigureOfMerit}.
#'
#' In addition to the core functionality described above, lulccR inludes several
#' utility functions to assist with the model building process. Two example
#' datasets are also included. 
#'
#' @author Simon Moulds
#' @docType package
#' @name lulccR-package
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
#' ## Complete example for Plum Island Ecosystems dataset
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
#' ## create PredModels objects
#' glm.models <- PredModels(models=list(forest.glm, built.glm, other.glm),
#'                          obs=obs)
#'
#' ## obtain demand scenario
#' dmd <- approxExtrapDemand(obs=obs, tout=c(0:14))
#'
#' ## create model input object
#' pie.model.input <- ModelInput(obs=obs,
#'                               ef=ef,
#'                               models=glm.models,
#'                               time=0:14,
#'                               demand=dmd)
#'
#' ## create ClueModel object
#' clues.rules <- matrix(data=c(1,1,1,
#'                              1,1,1,
#'                              1,1,1), nrow=3, ncol=3, byrow=TRUE)
#'
#' ## clues.parms <- list(jitter.f=0.0002,
#'                        scale.f=0.000001,
#'                        max.iter=1000,
#'                        max.diff=50,
#'                        ave.diff=50)
#'
#' pie.clues <- CluesModel(x=pie.model.input,
#'                         elas=c(0.2,0.2,0.2),
#'                         rules=clues.rules,
#'                         params=clues.parms)
#'
#' ## create OrderedModel object
#' ## allocate built land first, then forest, then other
#' pie.ordered <- OrderedModel(x=pie.model.input,
#'                             order=c(2,1,3)) 
#'
#' ## perform allocation
#' pie.clues <- allocate(pie.clues)
#' pie.ordered <- allocate(pie.ordered)
#'
#' ## validate ordered model input
#' pie.ordered.tabs <- ThreeMapComparison(x=pie.ordered,
#'                                        factors=2^(1:9),
#'                                        timestep=14)
#'
#' pie.ordered.agr <- AgreementBudget(x=pie.ordered.tabs, from=1, to=2)
#' p <- AgreementBudget.plot(x=pie.ordered.agr)
#' print(p)
#'
#' pie.ordered.fom <- FigureOfMerit(x=pie.ordered.tabs)
#' p <- FigureOfMerit.plot(x=pie.ordered.fom)
#' print(p)
#' }
NULL
