set.seed(1789)
filter.task <- makeClassifTask(data = trainingDataFilter, target = targetFilter)
print(filter.task)
lrn.rf <- makeLearner("classif.randomForest", predict.type="prob")
lrn.svm <- makeLearner("classif.svm", predict.type="prob")
lrn.knn <- makeLearner("classif.IBk", predict.type="prob")
lrn.nn <- makeLearner("classif.nnet", predict.type="prob")
lrn.multinom <- makeLearner("classif.multinom", predict.type="prob")
lrns <- list(lrn.rf, lrn.svm, lrn.knn, lrn.nn, lrn.multinom)
lrns <- list(lrn.rf, lrn.svm)
rdesc <- makeResampleDesc("CV", iters = 10, stratify = TRUE)
bmr <- benchmark(lrns, filter.task, rdesc, measures = list(acc, mmce, auc, ppv, tpr, fdr), keep.pred = TRUE)

AggrPerformances <- getBMRAggrPerformances(bmr, as.df = TRUE)
print(AggrPerformances)


# select learner
lrn <- lrn.rf 

# set hyper-parameter space
params <- makeParamSet(
	makeIntegerParam("mtry",lower = 1, upper = (ncol(trainingData) - 1) %/% 2)
	)
# set resampling strategy
rdesc <- makeResampleDesc("CV", iters = 10, stratify = TRUE)
# set optimization technique
ctrl <- makeTuneControlGrid(resolution=20L)
# start tuning
tune <- tuneParams(learner = lrn, task = filter.task, 
	resampling = rdesc, 
	measures = list(acc, mmce, auc, ppv, tpr, fdr), 
	par.set = params, 
	control = ctrl, 
	show.info = TRUE)

plotHyperParsEffect(generateHyperParsEffectData(tune), x = "mtry", y = "acc.test.mean",
  plot.type = "line")

lrn.rf$par.vals <- list(mtry = 6L)

set.seed(1789)
benchmark(lrn.rf, filter.task, rdesc, measures = list(acc, mmce, auc, ppv, tpr, fdr), keep.pred = TRUE)

modFilter <-  train(lrn.rf, filter.task)
predFilter <- predict(modFilter, task = filter.task)
cm <- calculateConfusionMatrix(predFilter, sums = TRUE)
cm <- t(cm$result)

tot_acc <- sum(diag(cm)[seq(nlevels(countryLabelFilter))]) / length(countryLabelFilter)
print(tot_acc)

precision <- (cm[, '-n-'] - cm[, '-err.-']) / cm[, '-n-']
recall <- (cm['-n-', ] - cm['-err.-', ]) / cm['-n-', ]
results <- data.frame(precision = precision[seq(ncol(trainingLabels))], recall = recall[seq(ncol(trainingLabels))])
results$false.discovery.rate <- 1 - results$precision
print(summary(results))

targetFilter <- as.matrix(obj_dtm)
colnames(targetFilter) <- colnames(validationDTM)
totPredFilter <- predict(modFilter, newdata = data.frame(targetFilter))
predRelevance <- totPredFilter$data$response