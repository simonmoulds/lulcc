## library(devtools)
## install_github("simonmoulds/r_lulccR", subdir = "lulccR")
## library(lulccR)

devtools::load_all()

## #############################################################################
## input data
## #############################################################################

data(pie)

## observed maps
obs <- ObsLulcMaps(x=pie,
                   pattern="lu", 
                   categories=c(1,2,3), 
                   labels=c("Forest","Built","Other"), 
                   t=c(0,6,14))

## explanatory variables
ef <- ExpVarMaps(x=pie, pattern="ef")

# obtain demand scenario
dmd <- approxExtrapDemand(obs=obs, tout=0:14)
matplot(dmd, type="l", ylab="Demand (no. of cells)", xlab="Time point")

# prepare model input
input <- ModelInput(obs=obs,
                    ef=ef,
                    time=0:14,
                    demand=dmd)

part <- partition(x=obs[[1]], size=0.1, spatial=TRUE)
train.data <- as.data.frame(x=input, cells=part[["train"]], t=0)
test.data  <- as.data.frame(x=input, cells=part[["test"]], t=0)

forms <- list(Built ~ ef_001+ef_002+ef_003,
              Forest ~ 1,
              Other ~ ef_001+ef_002)

glm.models <- glmModels(formula=forms, family=binomial, data=train.data, obs=obs)

glm.pred <- Prediction(models=glm.models, newdata=test.data)
glm.perf <- Performance(pred=glm.pred, measure="rch")
plot(list(glm=glm.perf))

## ## convert initial land use map to RasterBrick where each layer is a boolean map for the respective land use category
## br <- raster::layerize(obs[[1]]) 
## names(br) <- obs@labels

## # extract training and testing partition cells
## train.df <- raster::extract(x=br, y=part$train, df=TRUE)
## train.df <- cbind(train.df, as.data.frame(x=ef, cells=part$train))

## # fit models (note that a predictive model is required for each land use category)
## built.form <- Built ~ ef_001+ef_002+ef_003
## forest.form <- Forest ~ 1
## other.form <- Other ~ ef_001+ef_002
## ## built.glm <- glm(Built ~ ef_001+ef_002+ef_003, family=binomial, data=train.df)
## ## forest.glm <- glm(Forest ~ 1, family=binomial, data=train.df)
## ## other.glm <- glm(Other ~ ef_001+ef_002, family=binomial, data=train.df)

## # recursive partitioning and regression tree models
## built.rpart <- rpart::rpart(formula=built.formula, data=train.df, method="class")
## other.rpart <- rpart::rpart(formula=other.formula, data=train.df, method="class")

## # random forest models (WARNING: takes a long time!)
## built.rf <- randomForest::randomForest(formula=built.formula, data=train.df)
## other.rf <- randomForest::randomForest(formula=other.formula, data=train.df)

## glm.models <- PredModels(models=list(forest.glm, built.glm, other.glm), obs=obs)
			 
## rpart.models <- PredModels(models=list(built.rpart, other.rpart),
##                            categories=obs@categories[2:3],
##                            labels=obs@labels[2:3])

## rf.models <- PredModels(models=list(built.rf, other.rf),
##                         categories=obs@categories[2:3],
##                         labels=obs@labels[2:3])


## rpart.pred <- Prediction(models=rpart.models,
##                          obs=obs,
##                          ef=ef,
##                          partition=part$test)

## rf.pred <- Prediction(models=rf.models,
##                       obs=obs,
##                       ef=ef,
##                       partition=part$test)

## rpart.perf <- Performance(pred=rpart.pred, measure="rch")
## rf.perf <- Performance(pred=rf.pred, measure="rch")

# plot ROC curve
## p <- Performance.plot(list(glm=glm.perf, rpart=rpart.perf, rf=rf.perf))
## print(p)


## # prepare model input
## input <- ModelInput(obs=obs,
##                     ef=ef,
##                     ## models=glm.models,
##                     time=0:14,
##                     demand=dmd)



## get neighbourhood values
w <- matrix(data=1, nrow=3, ncol=3)
nb <- NeighbMaps(x=obs[[1]], weights=w, categories=2)

# CLUE-S
clues.rules <- matrix(data=c(1,1,1,1,1,1,1,1,1),
                      nrow=3, ncol=3, byrow=TRUE) 

clues.parms <- list(jitter.f=0.0002,
                    scale.f=0.000001,
                    max.iter=1000,
                    max.diff=50, 
                    ave.diff=50) 

clues.model <- CluesModel(x=input,
                          models=glm.models,
                          neighb=nb,
                          elas=c(0.2,0.2,0.2),
                          rules=clues.rules,
                          params=clues.parms)

## Ordered
ordered.model <- OrderedModel(x=input,
                              models=glm.models,
                              order=c(2,1,3)) 

## perform allocation
clues.model <- allocate(clues.model)
ordered.model <- allocate(ordered.model)

## evaluate CLUE-S model output
clues.tabs <- ThreeMapComparison(x=clues.model,
                                 timestep=14, 
                                 factors=2^(1:9))

## calculate agreement budget and plot
clues.agr <- AgreementBudget(x=clues.tabs)
plot(clues.agr, from=1, to=2)

## calculate Figure of Merit and plot
clues.fom <- FigureOfMerit(x=clues.tabs)
plot(clues.fom, from=1, to=2)

## ## Warnings
## This package and functions herein are part of an experimental open-source project. They are provided as is, without any guarantee.

## ## Please leave your feedback
## I would be grateful for any feedback on this project (simonm@riseup.net).
