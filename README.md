lulccR (R package)
=============================================

lulccR provides a framework for spatially explicit land use change modelling in R. The development version of lulccR is available on github and can be installed using devtools:

```R
library(devtools)
install_github("simonmoulds/r_lulccR", subdir = "lulccR")
library(lulccR)
```
# Data and functions

The package includes two example datasets: one for Sibuyan Island in the Phillipines and one for the Plum Island Ecosystem in Massachusetts, United States. Here we present a complete working example for the Plum Island Ecosystem dataset.

## Input maps
Land use change modelling requires a large amount of input data. The most important input is at least one map of observed land use. In lulccR, this data is represented by the `ObsLulcMaps` class:

```R
obs <- ObsLulcMaps(x=pie,
                   pattern="lu",
                   categories=c(1,2,3),
                   labels=c("forest","built","other"),
                   t=c(0,6,14)) 
```

A useful starting point in land use change modelling is to obtain a transition matrix for two observed land use maps to identify the main transitions. This can be achieved with the `crossTabulate` function:

```R
# obtain a transition matrix from land use maps for 1985 and 1991
crossTabulate(obs, index=c(1,2))
```

For the Plum Island Ecosystem site this reveals that the main transition was from forest to built areas.

The next stage is to relate observed land use or observed land use transitions to spatially explicit biophysical or socioeconomic explanatory variables. These are loaded as follows:

```R
ef.maps <- ExpVarMaps(x=pie, pattern="ef")
```

To fit predictive models we first divide the study region into training and testing partitions. The `partition` function returns a list with cell numbers for each partition:

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
ef.train.df <- as.data.frame(x=ef.maps, cells=part$train)
train.data <- cbind(lu.train.df, ef.train.df)

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

Predictive models are represented by the `PredModels` class. For comparison, we create a `PredModels` object for each type of predictive model: 

```R
glm.models <- PredModels(models=list(forest.glm, built.glm, other.glm),
                         obs=obs)
			 
rpart.models <- PredModels(models=list(built.rpart, other.rpart),
                           categories=obs@categories[2:3],
                           labels=obs@labels[2:3])

rf.models <- PredModels(models=list(built.rf, other.rf),
                        categories=obs@categories[2:3],
                        labels=obs@labels[2:3])
```

Model performance is assessed using the receiver operator characteristic provided by the [ROCR](http://cran.r-project.org/web/packages/ROCR/index.html) package. lulccR includes classes `Prediction` and `Performance` which extend the native ROCR classes to contain multiple `prediction` and `performance` objects. The procedure to obtain these objects and assess performance is as follows:

```R
# extract cell values for the testing partition
lu.test.df <- raster::extract(x=lu.br, y=part$test, df=TRUE)
pred.test.df <- as.data.frame(x=pred, cells=part$test)
test.data <- cbind(lu.test.df, pred.test.df)

glm.pred <- Prediction(models=glm.models,
                       obs=obs,
                       ef=ef,
                       partition=part$test)

rpart.pred <- Prediction(models=rpart.models,
                         obs=obs,
                         ef=ef,
                         partition=part$test)

rf.pred <- Prediction(models=rf.models,
                      obs=obs,
                      ef=ef,
                      partition=part$test)

glm.perf <- Performance(pred=glm.pred, measure="rch")
rpart.perf <- Performance(pred=rpart.pred, measure="rch")
rf.perf <- Performance(pred=rf.pred, measure="rch")

# plot ROC curve
p <- Performance.plot(list(glm=glm.perf, rpart=rpart.perf, rf=rf.perf))
print(p)
```

Spatially explicit land use change models are usually driven by non-spatial estimates of land use area for each timestep in the simulation. While many complex methods have been devised, in lulccR we simply provide a method for linear extrapolation of land use change, which relies on there being at least two observed land use maps:

```R
# obtain demand scenario
dmd <- approxExtrapDemand(obs=obs, tout=0:14)
```

The culmination of the modelling process is to simulate the location of land use change. lulccR provides a routine based on the CLUE-S model (Verburg et al., 2002) and a novel stochastic allocation procedure. The first step is to combine the various model inputs to ensure they are compatible:

```R
# prepare model input
input <- ModelInput(x=obs,
                    ef=ef,
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

clues.input <- CluesModel(x=input,
                          elas=c(0.2,0.2,0.2),
                          rules=clues.rules,
                          params=clues.parms)

# allocate built, then forest, then other
ordered.input <- OrderedModel(x=input, order=c(2,1,3))
```

Then, finally, we can perform allocation:

```R
clues.model <- allocate(clues.input)
ordered.model <- allocate(ordered.input)
```

An important yet frequently overlooked aspect of land use change modelling is model validation. lulccR provides a recent validation method developed by Pontius et al. (2011), which simultaneously compares a reference (observed) map for time 1, a reference map for time 2 and a simulated map for time 2. The first step in this method is to calculate three dimensional contingency tables:

```R
# evaluate CLUE-S model output
clues.tabs <- ThreeMapComparison(x=clues.model,
                                 factors=2^(1:10),
                                 timestep=14)
```

From these tables we can easily extract information about different types of agreement and disagreement as well as compute summary statistics such as the figure of merit:

```R
# calculate agreement budget and plot
clues.agr <- AgreementBudget(x=clues.tabs,from=1,to=2)
p <- AgreementBudget.plot(clues.agr)
print(p)

# calculate Figure of Merit and plot
clues.fom <- FigureOfMerit(x=clues.tabs)
p <- FigureOfMerit(clues.fom)
print(p)
```

## Warnings
This package and functions herein are part of an experimental open-source project. They are provided as is, without any guarantee.

## Please leave your feedback
I would be grateful for any feedback on this project (simonmdev@riseup.net).
