lulccR (R package)
=============================================

lulccR provides a framework for spatially explicit land use change modelling in R. It has been designed so that users can carry out every stage of the modelling process in the same environment. It includes two example datasets: Sibuyan Island (Phillipines) (Verburg et al., 2002) and the Plum Island Ecosystem (USA) (Pontius et al., 2014). Two allocation methods are provided: one based on the Change in Land Use and its Effects at Small regional extent (CLUE-S) model (Verburg et al., 2002) and a novel stochastic procedure. Predictive models form the basis of many land use change models by relating observed land use to biophysical and socioeconomic predictor variables. We provide support for binary logistic regression, recursive partitioning and regression trees and random forest models. These models can be validated with the receiver operator characteristic or any other metric provided by the [ROCR](http://cran.r-project.org/web/packages/ROCR/index.html) package. Finally, the package provides a validation method for the output of land use change models that was originally developed by Pontius et al. (2011). 

### Basics
The development version of lulccR is available on github and can be installed using devtools:

```R
library(devtools)
install_github("simonmoulds/r_lulccR", subdir = "lulccR")
library(lulccR)
```
# Data and functions

A complete working example for the Plum Island Ecosystem dataset is presented here.

First, load observed data into an `ObservedMaps` object:

```R
obs <- ObservedMaps(x=pie,
                    pattern="lu",
                    categories=c(1,2,3),
                    labels=c("forest","built","other"),
                    t=c(0,6,14)) 
```

Two types of predictor variables are allowed. The first type are usually biophysical or socioeconomic variables that can be related to land use, such as elevation (biophysical) or population density (socioeconomic). These are loaded as follows:

```R
pred.maps <- predictorMaps(x=pie, pattern="pred")
```

The second type of predictor variable is based on the observed pattern of land use itself, such as neighbourhood maps or maps showing the distance to a certain land use category. The `PredictorCall` class represents this type of predictor variable. Here, we create a variable based on distance to forested cells:

```R
# function to calculate distance to land use category
myfun <- function(x, category, ...) {
    y <- x
    y[!is.na(y)] <- 1
    x[x != category] <- NA
    dist <- raster::distance(x, ...)
    dist <- dist * y
}

# PredictorCall object for distance to forest
dist2forest <- PredictorCall(call=call("myfun", x=obs@maps[[1]], category=1),
                             update.arg="x",
			     name="dist2forest")
```

These two types of predictor variable are combined into a `Predictors` object:

```R
# example Predictors object with both maps and calls
pred <- Predictors(maps=pred.maps, calls=list(dist2forest))

# for the current application we do not use dist2forest, so only use maps
pred <- Predictors(maps=pred.maps)
```

To fit the predictive models we first divide the study region into training and testing partitions. The `partition` function returns a list with cell numbers for each partition:

```R
part <- partition(x=obs@maps[[1]], size=0.5, spatial=FALSE)
```

We then extract cell values for the training partition and create a data.frame to supply to the model fitting functions:

```R
# convert initial land use map to RasterBrick where each layer is a boolean
# map for the respective land use category
lu.br <- raster::layerize(obs@maps[[1]])
names(lu.br) <- obs@labels

# extract training and testing partition cells
lu.train.df <- raster::extract(x=lu.br, y=part$train, df=TRUE)
pred.train.df <- as.data.frame(x=pred, cells=part$train)
train.data <- cbind(lu.train.df, pred.train.df)

# fit models (note that a predictive model is required for each land use category)
forest.formula <- formula(forest ~ 1) # null model
built.formula <- formula(built ~ pred_001+pred_002+pred_003)
other.formula <- formula(other ~ pred_001+pred_002)

# glm models
forest.glm <- glm(formula=forest.formula, family=binomial, data=train.data)
built.glm <- glm(formula=built.formula, family=binomial, data=train.data)
other.glm <- glm(formula=other.formula, family=binomial, data=train.data)

# recursive partitioning and regression tree models
built.rpart <- rpart::rpart(formula=built.formula, data=train.data, method="class")
other.rpart <- rpart::rpart(formula=other.formula, data=train.data, method="class")

# random forest models (WARNING: takes a long time!)
built.rf <- randomForest::randomForest(formula=built.formula, data=train.data)
other.rf <- randomForest::randomForest(formula=other.formula, data=train.data)
```

The predictive models are represented by the `StatModels` class. For comparison, we create a `StatModels` object for each type of predictive model: 

```R
# create StatModels objects
glm.models <- StatModels(models=list(forest.glm, built.glm, other.glm), obs=obs)
rpart.models <- StatModels(models=list(built.rpart, other.rpart), categories=obs@categories[2:3], labels=obs@labels[2:3])
rf.models <- StatModels(models=list(built.rf, other.rf), categories=obs@categories[2:3], labels=obs@labels[2:3])
```

Model performance is assessed using the receiver operator characteristic provided by the [ROCR](http://cran.r-project.org/web/packages/ROCR/index.html) package. lulccR contains the classes `PredictionMulti` and `PerformanceMulti` which are designed to represent multiple `prediction` and `performance` objects. The procedure to obtain these objects and assess performance is as follows:

```R
# extract cell values for the testing partition
lu.test.df <- raster::extract(x=lu.br, y=part$test, df=TRUE)
pred.test.df <- as.data.frame(x=pred, cells=part$test)
test.data <- cbind(lu.test.df, pred.test.df)

glm.pred <- PredictionMulti(models=glm.models,
                            obs=obs,
                            pred=pred,
                            partition=part$test)

rpart.pred <- PredictionMulti(models=rpart.models,
                              obs=obs,
                              pred=pred,
                              partition=part$test)

rf.pred <- PredictionMulti(models=rf.models,
                           obs=obs,
                           pred=pred,
                           partition=part$test)

glm.perf <- PerformanceMulti(pred=glm.pred, measure="rch")
rpart.perf <- PerformanceMulti(pred=rpart.pred, measure="rch")
rf.perf <- PerformanceMulti(pred=rf.pred, measure="rch")

# plot ROC curve
p <- plot.roc(list(glm=glm.perf, rpart=rpart.perf, rf=rf.perf))
print(p)
```

```R
# obtain demand scenario
dmd <- approxExtrapDemand(obs=obs, tout=0:14)

# prepare model input
input <- ModelInput(x=obs,
                    pred=pred,
                    models=glm.models,
                    time=0:14,
                    demand=dmd)

clues.rules <- matrix(data=c(1,1,1,
                             1,1,1,
                             1,1,1), nrow=3, ncol=3, byrow=TRUE)

clues.parms <- list(jitter.f=0.0002,
                    scale.f=0.000001,
                    max.iter=1000,
                    max.diff=50,
                    ave.diff=50)

clues.input <- CluesModelInput(x=input,
                               elas=c(0.2,0.2,0.2),
                               rules=clues.rules,
                               params=clues.parms)

ordered.input <- OrderedModelInput(x=input)

# allocate land use change using two methods
clues.maps <- allocate(clues.input)
ordered.maps <- allocate(ordered.input)

# evaluate CLUE-S model output
clues.tabs <- ThreeMapComparison(rt1=obs@maps[[1]],
                                 rt2=obs@maps[[2]],
                                 st2=clues.maps[[15]],
                                 categories=obs@categories,
                                 labels=obs@labels,
                                 factors=2^(1:10))

# calculate agreement budget and plot
clues.agr <- AgreementBudget(x=clues.tabs,from=1,to=2)
p <- plot.agreement(clues.agr)

# calculate Figure of Merit and plot
clues.fom <- FigureOfMerit(x=clues.tabs)
p <- plot.fom(clues.fom)
```

## Warnings
This package and functions herein are part of an experimental open-source project. They are provided as is, without any guarantee.

## Please leave your feedback
I would be grateful for any feedback on this project (simonmdev@riseup.net).
