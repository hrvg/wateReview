# select learner
lrn <- lrn.rf 

# set hyper-parameter space
params <- makeParamSet(
	makeIntegerParam("mtry",lower = 1, upper = (ncol(trainingData) - 1) %/% 2)
	)

# set resampling strategy
rdesc <- makeResampleDesc("CV", iters = 5, stratify = TRUE)

# set optimization technique
ctrl <- makeTuneControlGrid(resolution=20L)

# start tuning
tune <- tuneParams(learner = lrn, task = scale.task, 
	resampling = rdesc, 
	measures = list(acc, mmce, multiclass.au1u, multiclass.aunu), 
	par.set = params, 
	control = ctrl, 
	show.info = TRUE)

plotHyperParsEffect(generateHyperParsEffectData(tune), x = "mtry", y = "acc.test.mean",
  plot.type = "line")