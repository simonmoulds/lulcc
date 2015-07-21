## Author: Simon Moulds
## Date: January 2015
## Version: 1.0
## Licence: GPL v3

## This script tests examples from lulccR

devtools::install_github("simonmoulds/r_lulccR", subdir = "lulccR")
library(lulccR)
 
setwd("/home/simon/projects/r_lulccR/lulccR")
roxygen2::update_collate(".")
devtools::load_all(quiet=TRUE)
devtools::document()

#################################################################################
## lulccR-package.R

## Load observed land use maps
obs <- ObsLulcMaps(x=pie,
                   pattern="lu",
                   categories=c(1,2,3),
                   labels=c("forest","built","other"),
                   t=c(0,6,14))

## Load explanatory variables
ef <- ExpVarMaps(x=pie, pattern="ef")

## create equally sized training and testing partitions
part <- partition(x=obs@maps[[1]], size=0.5, spatial=FALSE)
 
## convert initial land use map to RasterBrick where each layer is a boolean
## map for the respective land use
br <- raster::layerize(obs@maps[[1]])
names(br) <- obs@labels

## create data.frame to fit models
train.df <- raster::extract(x=br, y=part$train, df=TRUE)
train.df <- cbind(train.df, as.data.frame(x=ef, cells=part$train))
                    
## model formulas
forest.formula <- formula(forest ~ 1)
built.formula <- formula(built~ef_001+ef_002+ef_003)
other.formula <- formula(built~ef_001+ef_002)

## glm models
forest.glm <- glm(formula=forest.formula, family=binomial, data=train.df)
built.glm <- glm(formula=built.formula, family=binomial, data=train.df)
other.glm <- glm(formula=other.formula, family=binomial, data=train.df)

## create PredModels objects
glm.models <- PredModels(models=list(forest.glm, built.glm, other.glm),
                         obs=obs)

## obtain demand scenario
dmd <- approxExtrapDemand(obs=obs, tout=c(0:14))

## create model input object
pie.model.input <- ModelInput(obs=obs,
                              ef=ef,
                              models=glm.models,
                              time=0:14,
                              demand=dmd)

## create ClueModel object
clues.rules <- matrix(data=c(1,1,1,
                             1,1,1,
                             1,1,1), nrow=3, ncol=3, byrow=TRUE)

clues.parms <- list(jitter.f=0.0002,
                    scale.f=0.000001,
                    max.iter=1000,
                    max.diff=50,
                    ave.diff=50)

pie.clues <- CluesModel(x=pie.model.input,
                        elas=c(0.2,0.2,0.2),
                        rules=clues.rules,
                        params=clues.parms)

## create OrderedModel object
pie.ordered <- OrderedModel(x=pie.model.input,
                            order=c(2,1,3))  ## built, forest, other

## perform allocation
pie.clues <- allocate(pie.clues)
pie.ordered <- allocate(pie.ordered, stochastic=TRUE)

## validate ordered model input
pie.ordered.tabs <- ThreeMapComparison(x=pie.ordered,
                                       factors=2^(1:9),
                                       timestep=14)

pie.ordered.agr <- AgreementBudget(x=pie.ordered.tabs, from=1, to=2)
p <- AgreementBudget.plot(x=pie.ordered.agr)
print(p)

pie.ordered.fom <- FigureOfMerit(x=pie.ordered.tabs)
p <- FigureOfMerit.plot(x=pie.ordered.fom)
print(p)

#################################################################################
## AgreementBudget.R

## load clues.model for Sibuyan Island
sib.clues.model <- sibuyan$intermediate$clues.model

sib.clues.tables <- ThreeMapComparison(x=sib.clues.model,
                                       factors=2^(1:9),
                                       timestep=14)

sib.clues.agr <- AgreementBudget(x=sib.clues.tables)
p <- AgreementBudget.plot(x=sib.clues.agr)
print(p)

#################################################################################
## AgreementBudget.plot.R

## NO EXAMPLES

#################################################################################
## allocate.R

## load clues.model for Sibuyan Island
sib.clues.model <- sibuyan$intermediate$clues.model

## allocate
sib.clues.model <- allocate(sib.clues.model)

library(raster)
plot(sib.clues.model@output[[15]])

#################################################################################
## allow.R

obs <- ObsLulcMaps(x=pie,
                   pattern="lu",
                   categories=c(1,2,3),
                   labels=c("forest","built","other"),
                   t=c(0,6,14))

## get land use values
x <- getValues(obs@maps[[1]])
x <- x[!is.na(x)]

## create vector of arbitrary land use history values
hist <- sample(1:10, length(x), replace=TRUE)

## calculate demand and get change direction for first timestep
dmd <- approxExtrapDemand(obs=obs, tout=0:14)
cd <- dmd[2,] - dmd[1,]
 
## create rules matrix, only allowing forest to change if the cell has
## belonged to forest for more than 8 years
rules <- matrix(data=c(1,1008,1008,
                        1,1,1,
                        1,1,1), nrow=3, ncol=3, byrow=TRUE)

allow <- allow(x=x,
               hist=hist,
               categories=obs@categories,
               cd=cd,
               rules=rules)

## create raster showing cells that are allowed to change from forest to built
r <- obs@maps[[1]]
r[!is.na(r)] <- allow[,2]
r[obs@maps[[1]] != 1] <- NA
plot(r)

## h <- obs@maps[[1]]
## h[!is.na(h)] <- hist
## h[h < 8] <- NA
## h[obs@maps[[1]] != 1] <- NA
## plot(h)

#################################################################################
## allowNeighb.R

## load observed land use data
obs <- ObsLulcMaps(x=pie,
                     pattern="lu",
                     categories=c(1,2,3),
                     labels=c("forest","built","other"),
                     t=c(0,6,14))

## create a NeighbMaps object for forest only
nb <- NeighbMaps(x=obs@maps[[1]],
                  categories=1,
                  weights=3,
                  fun=mean)

## only allow change to forest within neighbourhood of current forest cells
## note that rules can be any value between zero (less restrictive) and one
## (more restrictive)
nb.allow <- allowNeighb(neighb=nb,
                        x=obs@maps[[1]],
                        categories=obs@categories,
                        rules=0.5)

## create raster showing cells allowed to change to forest
r <- obs@maps[[1]]
r[!is.na(r)] <- nb.allow[,1]
plot(r)

## NB output is only useful when used within an allocation routine

#################################################################################
## approxExtrapDemand.R

## load observed land use maps
obs <- ObsLulcMaps(x=pie,
                   pattern="lu",
                   categories=c(1,2,3),
                   labels=c("forest","built","other"),
                   t=c(0,6,14))

## obtain demand scenario
dmd <- approxExtrapDemand(obs=obs, tout=c(0:14))

#################################################################################
## as.data.frame.R

ef <- ExpVarMaps(x=pie, pattern="ef")
part <- partition(x=obs@@maps[[1]], size=0.5, spatial=FALSE)
efdf <- as.data.frame(x=ef, cells=part$train)

#################################################################################
## calcProb.R

## load observed land use data
obs <- ObsLulcMaps(x=sibuyan$maps,
                    pattern="lu",
                    categories=c(1,2,3,4,5),
                    labels=c("forest","coconut","grass","rice","other"),
                    t=c(0,14))

## load explanatory variables
ef <- ExpVarMaps(x=sibuyan$maps, pattern="ef")
part <- partition(x=obs@maps[[1]], size=0.5, spatial=FALSE)
efdf <- as.data.frame(x=ef, cells=part$all)

## get glm.models from data
glm.models <- sibuyan$intermediate$glm.models

probmaps <- calcProb(object=glm.models, newdata=efdf, df=TRUE)
points <- rasterToPoints(obs@maps[[1]], spatial=TRUE)
probmaps <- SpatialPointsDataFrame(coords=points, data=probmaps)
r <- stack(rasterize(x=probmaps, y=obs@maps[[1]], field=names(probmaps)))
plot(r)

#################################################################################
## crossTabulate.R

## Plum Island Ecosystems 

## Load observed land use maps
obs <- ObsLulcMaps(x=pie,
                   pattern="lu",
                   categories=c(1,2,3),
                   labels=c("forest","built","other"),
                   t=c(0,6,14))

crossTabulate(x=obs, index=c(1,3))

## RasterLayer input
crossTabulate(x=pie$lu_pie_1985,
              y=pie$lu_pie_1999,
              categories=c(1,2,3),
              labels=c("forest","built","other"))

#################################################################################
## ExpVarMaps.R

## Plum Island Ecosystems
ef <- ExpVarMaps(x=pie, pattern="ef")

## Sibuyan
ef <- ExpVarMaps(x=sibuyan$maps, pattern="ef")

#################################################################################
## NeighbMaps.R

## observed data
obs <- ObsLulcMaps(x=pie,
                    pattern="lu",
                    categories=c(1,2,3),
                    labels=c("forest","built","other"),
                    t=c(0,6,14))

## create a NeighbMaps object for 1985 land use map
nb1 <- NeighbMaps(x=obs@maps[[1]],     
                  categories=c(1,2,3), # all land use categories
                  weights=c(3,3,3))           # 3*3 neighbourhood

w1 <- matrix(data=c(1,1,1,
                    1,1,1,
                    1,1,1), nrow=3, ncol=3, byrow=TRUE)

w2 <- matrix(data=c(1,1,1,
                    1,1,1,
                    1,1,1), nrow=3, ncol=3, byrow=TRUE)

w3 <- matrix(data=c(1,1,1,
                    1,1,1,
                    1,1,1), nrow=3, ncol=3, byrow=TRUE)

nb2 <- NeighbMaps(x=obs@maps[[1]],
                  categories=c(1,2,3),
                  weights=list(w1,w2,w3))

## update nb2 for 1991
nb2 <- NeighbMaps(x=obs@maps[[2]],
                  neighb=nb2)

## plot neighbourhood map for forest
plot(nb2@maps[[1]])

#################################################################################
## ObsLulcMaps.R

## Plum Island Ecosystems
obs <- ObsLulcMaps(x=pie,
                   pattern="lu",
                   categories=c(1,2,3),
                   labels=c("forest","built","other"),
                   t=c(0,6,14))

## Sibuyan Island
obs <- ObsLulcMaps(x=sibuyan$maps,
                   pattern="lu",
                   categories=c(1,2,3,4,5),
                   labels=c("forest","coconut","grass","rice","other"),
                   t=c(0,14))

#################################################################################
## partition.R

## Plum Island Ecosystems

## Load observed land use maps
obs <- ObsLulcMaps(x=pie,
                   pattern="lu",
                   categories=c(1,2,3),
                   labels=c("forest","built","other"),
                   t=c(0,6,14))

## create equally sized training and testing partitions
part <- partition(x=obs@maps[[1]], size=0.5, spatial=FALSE)

#################################################################################
## PredModels.R
 
## Plum Island Ecosystems

## Load observed land use maps
obs <- ObsLulcMaps(x=pie,
                   pattern="lu",
                   categories=c(1,2,3),
                   labels=c("forest","built","other"),
                   t=c(0,6,14))

## Load explanatory variables
ef <- ExpVarMaps(x=pie, pattern="ef")

## create equally sized training and testing partitions
part <- partition(x=obs@maps[[1]], size=0.5, spatial=FALSE)

## convert initial land use map to RasterBrick where each layer is a boolean
## map for the respective land use
br <- raster::layerize(obs@maps[[1]])
names(br) <- obs@labels

## create data.frame to fit models
train.df <- raster::extract(x=br, y=part$train, df=TRUE)
train.df <- cbind(train.df, as.data.frame(x=ef, cells=part$train))

## model formulas
forest.formula <- formula(forest ~ 1)
built.formula <- formula(built~ef_001+ef_002+ef_003)
other.formula <- formula(built~ef_001+ef_002)

## glm models
forest.glm <- glm(formula=forest.formula, family=binomial, data=train.df)
built.glm <- glm(formula=built.formula, family=binomial, data=train.df)
other.glm <- glm(formula=other.formula, family=binomial, data=train.df)

## NB rpart and randomForest do not accept null model formula, so only fit
## models for 'built' and 'other' classes

## rpart models
built.rp <- rpart::rpart(formula=built.formula, data=train.df, method="class")
other.rp <- rpart::rpart(formula=other.formula, data=train.df, method="class")

## random forest models
built.rf <- randomForest::randomForest(formula=built.formula, data=train.df)
other.rf <- randomForest::randomForest(formula=other.formula, data=train.df)

## create PredModels object
glm.models <- PredModels(models=list(forest.glm, built.glm, other.glm),
                         obs=obs)

rp.models <- PredModels(models=list(built.rp, other.rp),
                        categories=obs@categories[2:3],
                        labels=obs@labels[2:3])

rf.models <- PredModels(model=list(built.rf, other.rf),
                        categories=obs@categories[2:3],
                        labels=obs@labels[2:3])

## obtain Prediction objects
glm.pred <- Prediction(models=glm.models, obs=obs, ef=ef, partition=part$test)
rp.pred <- Prediction(models=rp.models, obs=obs, ef=ef, partition=part$test)
rf.pred <- Prediction(models=rf.models, obs=obs, ef=ef, partition=part$test)

## quickly compare area under the curve
compareAUC(pred=list(glm=glm.pred, rpart=rp.pred, randomForest=rf.pred))

## obtain Performance objects
glm.perf <- Performance(pred=glm.pred, measure="rch")
rp.perf <- Performance(pred=rp.pred, measure="rch")
rf.perf <- Performance(pred=rf.pred, measure="rch")

## plot ROC curve
p <- Performance.plot(list(glm=glm.perf, rpart=rp.perf, rf=rf.perf),
                      layout=c(3,1),
                      aspect="iso",
                      xlab=list(label=""),
                      ylab=list(label=""),
                      scales=list(cex=0.6),
                      key.args=list(cex=0.4, size=2.5),
                      par.strip.text=list(cex=0.6),
                      par.settings=list(strip.background=
                                          list(col="lightgrey")))

## view plot
print(p)

#################################################################################
## resample.R

## create ExpVarMaps object
ef <- ExpVarMaps(x=pie, pattern="ef")

## resample to ensure maps have same characteristics as observed maps
ef <- resample(x=ef, y=pie$lu_pie_1985, method="ngb")

#################################################################################
## roundSum

## load demand scenario from data
dmd <- sibuyan$demand$demand1 * runif(1)
ncell <- length(which(!is.na(getValues(sibuyan$maps$lu_sib_1997))))

## recover demand
dmd <- roundSum(dmd, ncell=ncell)

#################################################################################
## ThreeMapComparison.R

## get CluesModel object from data
sib.clues.model <- sibuyan$intermediate$clues.model

## validation
sib.clues.tables <- ThreeMapComparison(x=sib.clues.model,
                                       factors=2^(1:9),
                                       timestep=14)

sib.clues.agr <- AgreementBudget(x=sib.clues.tables)
p <- AgreementBudget.plot(x=sib.clues.agr)
print(p)

sib.clues.fom <- FigureOfMerit(x=sib.clues.tables)
p <- FigureOfMerit.plot(x=sib.clues.fom)
print(p)

#################################################################################
## total.R

## RasterLayer
total(x=sibuyan$maps$lu_sib_1997)

## RasterStack
total(x=stack(pie$lu_pie_1985, pie$lu_pie_1991, pie$lu_pie_1999))
