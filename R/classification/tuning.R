
#set parameter space
params <- makeParamSet(makeIntegerParam("mtry",lower = 1,upper = 100))
#set validation strategy
params <- makeParamSet()
rdesc <- makeResampleDesc("CV",iters = 5)
#set optimization technique
ctrl <- makeTuneControlRandom(maxit = 20)
#start tuning
tune <- tuneParams(learner = lrn, task = scale.task, resampling = rdesc, measures = list(acc), par.set = getParamSet(lrn), control = ctrl, show.info = T)
plotHyperParsEffect(generateHyperParsEffectData(tune), x = "iteration", y = "acc.test.mean",
  plot.type = "line")