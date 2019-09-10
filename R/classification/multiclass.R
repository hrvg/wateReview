set.seed(1789)
scale.task <- makeClassifTask(data = trainingData, target = target)
print(scale.task)
lrn.rf <- makeLearner("classif.randomForest", predict.type="prob")
lrn.svm <- makeLearner("classif.svm", predict.type="prob")
lrn.knn <- makeLearner("classif.IBk", predict.type="prob")
lrn.nn <- makeLearner("classif.nnet", predict.type="prob")
lrn.multinom <- makeLearner("classif.multinom", predict.type="prob")
lrns <- list(lrn.rf, lrn.svm, lrn.knn, lrn.nn, lrn.multinom)
rdesc <- makeResampleDesc("CV", iters = 5, stratify = TRUE)
bmr <- benchmark(lrns, scale.task, rdesc, measures = list(acc, mmce, multiclass.au1u, multiclass.aunu), keep.pred = TRUE)

AggrPerformances <- getBMRAggrPerformances(bmr, as.df = TRUE)
print(AggrPerformances)

lrn.rf$par.vals <- list(mtry = 6L)

benchmark(lrn.rf, scale.task, rdesc, measures = list(acc, mmce, multiclass.au1u, multiclass.aunu), keep.pred = TRUE)

mod <-  train(lrn.rf, scale.task)
pred <- predict(mod, task = scale.task)
cm <- calculateConfusionMatrix(pred, sums = TRUE)
cm <- t(cm$result)

tot_acc <- sum(diag(cm)[seq(ncol(trainingLabels))]) / nrow(trainingLabels)
print(tot_acc)

precision <- (cm[, '-n-'] - cm[, '-err.-']) / cm[, '-n-']
recall <- (cm['-n-', ] - cm['-err.-', ]) / cm['-n-', ]
results <- data.frame(precision = precision[seq(ncol(trainingLabels))], recall = recall[seq(ncol(trainingLabels))])
results$false.discovery.rate <- 1 - results$precision
print(summary(results))