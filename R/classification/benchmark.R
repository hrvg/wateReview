binary.learner <- makeLearner("classif.svm", predict.type="prob")
binary.learner <- makeLearner("classif.rpart", predict.type="prob")
lrn.rfsrc <- makeLearner("multilabel.randomForestSRC", predict.type="prob")
lrn.rFerns <- makeLearner("multilabel.rFerns", predict.type="prob")

lrn.br <- makeMultilabelBinaryRelevanceWrapper(binary.learner)
# chainingOrder <- c("years_100000", "years_10000", "years_1000", "years_100", "day", "week", "event", "years_10", "year")
# chainingOrder <- c("very_long_term", "short_term", "long_term")
chainingOrder <- row.names(MLDR$labels)[order(MLDR$labels$count, decreasing = TRUE)]
lrn.cc <- makeMultilabelClassifierChainsWrapper(binary.learner, order = chainingOrder)
lrn.db <- makeMultilabelDBRWrapper(binary.learner)
lrn.ns <- makeMultilabelNestedStackingWrapper(binary.learner, order = chainingOrder)
lrn.st <- makeMultilabelStackingWrapper(binary.learner)

lrns <- list(lrn.rfsrc, lrn.rFerns, lrn.br)
# lrns <- list(lrn.rfsrc, lrn.rFerns, lrn.br, lrn.cc, lrn.db, lrn.ns, lrn.st)
# lrns <- list(lrn.rfsrc, lrn.rFerns, lrn.br, lrn.cc)
lrns <- list(lrn.br, lrn.cc, lrn.db, lrn.ns, lrn.st)

lrns <- list(lrn.cc, lrn.rfsrc)

set.seed(753)
# rdesc <- makeResampleDesc("RepCV", folds = 3, reps = 10)
# library("parallelMap")
# parallelStartMulticore(6, level = "mlr.resample")
# parallelStartSocket("multicore", 6, level = "mlr.resample") # choosing the paralellization at the resample level

rdesc <- makeResampleDesc("Subsample", iters = 3, split = 3 / 4)

bmr <- benchmark(lrns, scale.task, rdesc, measures = list(multilabel.hamloss, multilabel.subset01, multilabel.acc, multilabel.tpr, multilabel.ppv, multilabel.f1), keep.pred = TRUE)

# parallelStop()

AggrPerformances <- getBMRAggrPerformances(bmr, as.df = TRUE)
print(AggrPerformances)

mes <- c("acc", "ppv", "tpr", "tnr", "auc")
MultilabelBinaryPerformance <- lapply(seq_along(lrns), function(i){
	perf <- getMultilabelBinaryPerformances(getBMRPredictions(bmr)[[1]][[i]], measures = list(acc, ppv, tpr, tnr, auc))
	dim(perf) <- c(length(target), length(mes))
	rownames(perf) <- target
	colnames(perf) <- mes
	perf <- cbind(perf, count = MLDR$labels$count)
})
names(MultilabelBinaryPerformance) <- sapply(lrns, function(lrn) lrn$id)
print(MultilabelBinaryPerformance)
# colnames(MultilabelBinaryPerformance) <- c("RFSRC")
# colnames(MultilabelBinaryPerformance) <- c("RFSRC", "rFern")
# colnames(MultilabelBinaryPerformance) <- c("RFSRC", "rFern", "BR", "DBR", "NS", "Stack")
# colnames(MultilabelBinaryPerformance) <- c("RFSRC", "rFern", "BR", "CC")


get_short_long_term_pred <- function(lrn){
	rfpred <- getBMRPredictions(bmr, as.df = TRUE, learner.ids = lrn$id)
	short_term_col.truth <- c("truth.event", "truth.day", "truth.week")
	long_term_col.truth <-  c("truth.years_10", "truth.years_100", "truth.years_1000", "truth.years_10000", "truth.years_100000")

	short_term.truth <- apply(rfpred[, colnames(rfpred) %in% short_term_col.truth], 1, function(row) any(row == TRUE))
	long_term.truth <- apply(rfpred[, colnames(rfpred) %in% long_term_col.truth], 1, function(row) any(row == TRUE))

	short_term_col.response <- c("response.event", "response.day", "response.week")
	long_term_col.response <-  c("response.years_10", "response.years_100", "response.years_1000", "response.years_10000", "response.years_100000")

	short_term.response <- apply(rfpred[, colnames(rfpred) %in% short_term_col.response], 1, function(row) any(row == TRUE))
	long_term.response <- apply(rfpred[, colnames(rfpred) %in% long_term_col.response], 1, function(row) any(row == TRUE))
	print(table(short_term.response == short_term.truth))
	print(table(long_term.response == long_term.truth))
}
