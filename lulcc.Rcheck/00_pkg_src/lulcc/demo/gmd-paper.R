## July 2015

## Demo script to run the code and produce the figures shown in the Geoscientific
## Model Development Discussions paper

library(RColorBrewer)  ## required for plotting

## load data
data(pie)

## observed maps
obs <- ObsLulcRasterStack(x=pie,
                          pattern="lu", 
                          categories=c(1,2,3), 
                          labels=c("Forest","Built","Other"), 
                          t=c(0,6,14))

## show object
obs

## figure 3
p <- plot(obs,
          between=list(x=0,y=0),
          par.settings=list(axis.line=list(col="black"),
            strip.background=list(col="lightgrey")),
          par.strip.text=list(cex=0.6),
          scales=list(cex=0.6),
          col.regions=c("palegreen","midnightblue","indianred1"),
          colorkey=FALSE,
          layout=c(3,1),
          key=list(space="bottom",
            cex=0.6,
            rectangles=list(col=c("palegreen","midnightblue","indianred1"), size=3),
            text=list(labels=c("Forest","Built","Other"))))
p

## library(lattice)
## trellis.device(device="pdf", width=4.72, height=3, file="f03_pie.pdf", family="Courier")
## print(p)
## dev.off()

## cross tabulate change between two time points
crossTabulate(obs, times=c(0,14))

## explanatory variables
ef <- ExpVarRasterList(x=pie, pattern="ef")
ef

## resample ef (not necessary in this example)
ef <- resample(ef, obs)

## fit statistical models
part <- partition(x=obs[[1]], size=0.1, spatial=TRUE)
train.data <- getPredictiveModelInputData(obs=obs, ef=ef, cells=part[["train"]])

forms <- list(Built ~ ef_001+ef_002+ef_003,
              Forest ~ ef_001+ef_002,
              Other ~ ef_001+ef_002)

glm.models <- glmModels(formula=forms, family=binomial, data=train.data, obs=obs)
rpart.models <- rpartModels(formula=forms, data=train.data, obs=obs)
rf.models <- randomForestModels(formula=forms, data=train.data, obs=obs)

## create suitability maps
all.data <- as.data.frame(x=ef, cells=part[['all']])
probmaps <- predict(object=glm.models, newdata=all.data, data.frame=TRUE)
points <- rasterToPoints(obs[[1]], spatial=TRUE)
probmaps <- SpatialPointsDataFrame(points, probmaps)
r <- rasterize(x=probmaps, y=obs[[1]], field=names(probmaps))

## figure 4
p <- rasterVis::levelplot(r,
                          layout=c(3,1),
                          margin=FALSE,
                          par.strip.text=list(cex=0.6),
                          par.settings=list(axis.line=list(col="black"),
                            strip.background=list(col="lightgrey")),
                          between=list(x=0,y=0),
                          col.regions=colorRampPalette(brewer.pal(9, "YlGnBu")),
                          at=seq(0,1,length=100),
                          scales=list(cex=0.6),
                          colorkey=list(space="bottom",labels=list(cex=0.6),width=0.5))
p

## trellis.device(device="pdf", width=4.72, height=3, file="f04_suitability.pdf", family="Courier")
## print(p)
## dev.off()

## test ability of models to predict location of forest, urban, other land uses in testing partition
test.data  <- getPredictiveModelInputData(obs=obs, ef=ef, cells=part[["test"]])
glm.pred <- PredictionList(models=glm.models, newdata=test.data)
glm.perf <- PerformanceList(pred=glm.pred, measure="rch")

rpart.pred <- PredictionList(models=rpart.models, newdata=test.data)
rpart.perf <- PerformanceList(pred=rpart.pred, measure="rch")

rf.pred <- PredictionList(models=rf.models, newdata=test.data)
rf.perf <- PerformanceList(pred=rf.pred, measure="rch")

## figure 5
p <- plot(list(glm=glm.perf, rpart=rpart.perf, rf=rf.perf),
          layout=c(3,1),
          aspect="iso",
          xlab=list(label="False Alarms/(False Alarms + Correct Rejections)", cex=0.6),
          ylab=list(label="Hits/(Hits + Misses)", cex=0.6),
          scales=list(x=list(tck=0.6), y=list(tck=0.6), cex=0.6),
          key.args=list(cex=0.3, size=1.5),
          par.strip.text=list(cex=0.6),
          par.settings=list(strip.background=list(col="lightgrey")))
p

## trellis.device(device="pdf", width=4.72, height=3, file="f05_roc.pdf", family="Courier")
## print(p)
## dev.off()

## test ability of models to predict location of urban gain 1985 -> 1991
part <- rasterToPoints(obs[[1]], fun=function(x) x != 2, spatial=TRUE)
test.data <- getPredictiveModelInputData(obs=obs, ef=ef, cells=part, t=6)

glm.pred <- PredictionList(models=glm.models[[2]], newdata=test.data)
glm.perf <- PerformanceList(pred=glm.pred, measure="rch")

## figure 6
p <- plot(list(glm=glm.perf),
          aspect="iso",
          xlab=list(label="False Alarms/(False Alarms + Correct Rejections)", cex=0.6),
          ylab=list(label="Hits/(Hits + Misses)", cex=0.6),
          scales=list(x=list(tck=0.6), y=list(tck=0.6), cex=0.6),
          key.args=list(cex=0.6, size=2.5),
          par.strip.text=list(cex=0.6),
          par.settings=list(strip.background=list(col="lightgrey")))
p

## trellis.device(device="pdf", width=3.27, height=3.27, file="f06_builtgain.pdf", family="Courier")
## print(p)
## dev.off()

## obtain demand scenario
dmd <- approxExtrapDemand(obs=obs, tout=0:14)

## plot demand scenario (figure not shown in paper)
matplot(dmd, type="l", ylab="Demand (no. of cells)", xlab="Time point", lty=1, col=c("Green","Red","Blue"))
legend("topleft", legend=obs@labels, col=c("Green","Red","Blue"), lty=1)

## neighbourhood values
w <- matrix(data=1, nrow=3, ncol=3)
nb <- NeighbRasterStack(x=obs[[1]], weights=w, categories=c(1,2,3))

## create CLUE-S model object
clues.rules <- matrix(data=1, nrow=3, ncol=3, byrow=TRUE) 

clues.parms <- list(jitter.f=0.0002,
                    scale.f=0.000001,
                    max.iter=1000,
                    max.diff=50, 
                    ave.diff=50) 

clues.model <- CluesModel(obs=obs,
                          ef=ef,
                          models=glm.models,
                          time=0:14,
                          demand=dmd,
                          elas=c(0.2,0.2,0.2),
                          rules=clues.rules,
                          params=clues.parms)

## Create Ordered model (Fuchs et al)
ordered.model <- OrderedModel(obs=obs,
                              ef=ef,
                              models=glm.models,
                              time=0:14,
                              demand=dmd,
                              order=c(2,1,3)) 

## perform allocation
clues.model <- allocate(clues.model)
ordered.model <- allocate(ordered.model, stochastic=TRUE)

## validate ordered model input

## CLUE-S
clues.tabs <- ThreeMapComparison(x=clues.model,
                                 factors=2^(1:8),
                                 timestep=14)

## plot three dimensional tables in different ways (figures not shown in paper)
plot(clues.tabs)
plot(clues.tabs, category=1, factors=2^(1:8)[c(1,3,5,7)])

## Ordered
ordered.tabs <- ThreeMapComparison(x=ordered.model,
                                   factors=2^(1:8),
                                   timestep=14)

## as above for CLUE-S model
plot(ordered.tabs)
plot(ordered.tabs, category=1, factors=2^(1:9)[c(1,3,5,7)])

## calculate agreement budget and plot

## CLUE-S
clues.agr <- AgreementBudget(x=clues.tabs)
p1 <- plot(clues.agr,
           from=1,
           to=2,
           par.strip.text=list(cex=0.6),
           par.settings=list(strip.background=list(col="lightgrey")),
           xlab=list(cex=0.6),
           ylab=list(cex=0.6),
           ylim=c(0,0.08),
           scales=list(y=list(at=c(seq(from=0,to=0.08,by=0.02)), cex=0.6, tck=0.6), x=list(cex=0.6, tck=0.6)),
           key=list(cex=0.6))
p1

## Ordered
ordered.agr <- AgreementBudget(x=ordered.tabs)
p2 <- plot(ordered.agr,
           from=1,
           to=2,
           par.strip.text=list(cex=0.6),
           par.settings=list(strip.background=list(col="lightgrey")),
           xlab=list(cex=0.6),
           ylab=list(cex=0.6),
           ylim=c(0,0.08),
           scales=list(y=list(at=c(seq(from=0,to=0.08,by=0.02)), cex=0.6, tck=0.6), x=list(cex=0.6, tck=0.6)),
           key=list(cex=0.6))
p2

## figure 7
agr.p <- c("CLUE-S"=p1, Ordered=p2, layout=c(1,2))
agr.p

## trellis.device(device="pdf", width=4.72, height=5.72, file="/home/simon/projects/lulccR_paper/RevisedDraft/f07_agreement.pdf")
## print(agr.p)
## dev.off()

## calculate Figure of Merit and plot

## CLUE-S
clues.fom <- FigureOfMerit(x=clues.tabs)
p1 <- plot(clues.fom,
           from=1,
           to=2,
           par.strip.text=list(cex=0.6),
           par.settings=list(strip.background=list(col="lightgrey")),
           xlab=list(cex=0.6),
           ylab=list(cex=0.6),
           ylim=c(0,1),
           scales=list(y=list(at=(seq(from=0,to=1,by=0.2)), cex=0.6), x=list(cex=0.6)),
           key=NULL)
p1

## Ordered
ordered.fom <- FigureOfMerit(x=ordered.tabs)
p2 <- plot(ordered.fom,
           from=1,
           to=2,
           par.strip.text=list(cex=0.6),
           par.settings=list(strip.background=list(col="lightgrey")),
           xlab=list(cex=0.6),
           ylab=list(cex=0.6),
           ylim=c(0,1),
           scales=list(y=list(at=(seq(from=0,to=1,by=0.2)), cex=0.6), x=list(cex=0.6)),
           key=NULL)
p2

fom.p <- c("CLUE-S"=p1, Ordered=p2, layout=c(1,2))
fom.p

## trellis.device(device="pdf", width=4.72, height=4.72, file="/home/simon/projects/lulccR_paper/RevisedDraft/f08_figure_of_merit.pdf")
## print(fom.p)
## dev.off()

