# select learner
lrn <- makeLearner("classif.randomForest", predict.type = "prob") 
ps <- makeParamSet(
	makeIntegerParam("mtry",lower = 1, upper = (ncol(trainingDataMulticlassFilter) - 1) %/% 2)
	)
ctrl <- makeTuneControlGrid(resolution=20L)
rdesc <- makeResampleDesc("CV", iters = 10, stratify = TRUE)

mes <- list(auc, mmce, acc, ppv, tpr, fdr)
parallelStartSocket(8, level = "mlr.resample", load.balancing = TRUE)
clusterSetRNGStream(iseed = 1789)

# start tuning
tune <- tuneParams(learner = lrn, task = learning.task, 
	resampling = rdesc, 
	measures = mes, 
	par.set = ps, 
	control = ctrl, 
	show.info = TRUE)
parallelStop()

plotHyperParsEffect(generateHyperParsEffectData(tune), x = "mtry", y = "auc.test.mean",
  plot.type = "line")