leg_validationDTM <- validationDTM
leg_trainingLabels <- trainingLabels

validationDTM <- rbind(leg_validationDTM, webscrapped_validationDTM)
trainingLabels <- rbind(leg_trainingLabels, webscrapped_trainingLabels)

trainingData <- cbind(validationDTM, trainingLabels)
MLDR <- mldr_from_dataframe(trainingData, 
	labelIndices = which(colnames(trainingData) %in% colnames(trainingLabels)), 
	name = "MLDR")

chainingOrder <- row.names(MLDR$labels)[order(MLDR$labels$count, decreasing = TRUE)]	

trainingLabels <- trainingLabels[, order(MLDR$labels$count, decreasing = FALSE)]
trainingLabels <- trainingLabels[, which(sort(MLDR$labels$count, decreasing = FALSE) >= 10)]

countryLabel <- apply(trainingLabels, 1, function(row) which(row == TRUE)[1])

validationDTM <- validationDTM[!is.na(countryLabel), ]
validationDTM  <- as.data.frame(validationDTM)
countryLabel <- countryLabel[!is.na(countryLabel)]
countryLabel <- colnames(trainingLabels)[countryLabel]
countryLabel <- as.factor(countryLabel)

trainingData <- cbind(validationDTM, countryLabel)
target <- "countryLabel"

dim(trainingData)

set.seed(1789)
scale.task <- makeClassifTask(data = trainingData, target = target)
scale.task
lrn.rf <- makeLearner("classif.randomForest", predict.type="prob")
lrn.svm <- makeLearner("classif.svm", predict.type="prob")
lrn.knn <- makeLearner("classif.IBk", predict.type="prob")
lrns <- list(lrn.rf, lrn.svm, lrn.knn)
rdesc <- makeResampleDesc("CV", iters = 5, stratify = TRUE)
bmr <- benchmark(lrns, scale.task, rdesc, measures = list(acc, mmce, multiclass.au1u, multiclass.aunu), keep.pred = TRUE)

AggrPerformances <- getBMRAggrPerformances(bmr, as.df = TRUE)
print(AggrPerformances)

mod <-  train(lrn.rf, scale.task)
pred <- predict(mod, task = scale.task)
cm <- calculateConfusionMatrix(pred, sums = TRUE)
cm <- t(cm$result)

acc <- sum(diag(cm)[seq(ncol(trainingLabels))]) / nrow(trainingLabels)

precision <- (cm[, '-n-'] - cm[, '-err.-']) / cm[, '-n-']
recall <- (cm['-n-', ] - cm['-err.-', ]) / cm['-n-', ]
results <- data.frame(precision = precision[seq(ncol(trainingLabels))], recall = recall[seq(ncol(trainingLabels))])
results$false.discovery.rate <- 1 - results$precision
summary(results)