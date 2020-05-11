binary.learner <- makeLearner("classif.svm")
lrn.cc <- makeMultilabelClassifierChainsWrapper(binary.learner)
lrn.br <- makeMultilabelBinaryRelevanceWrapper(binary.learner)
lrn.ns <- makeMultilabelNestedStackingWrapper(binary.learner)
lrn.db <- makeMultilabelDBRWrapper(binary.learner)
lrn.st <- makeMultilabelStackingWrapper(binary.learner)

lrns <- list(lrn.cc, lrn.br, lrn.ns, lrn.db, lrn.st)

for (lrn in lrns){
	scale.mod <- train(lrn, scale.task, subset = train.set)
	scale.pred <- predict(scale.mod, task = scale.task, subset = test.set)
	print(performance(scale.pred, measures = list(multilabel.acc, multilabel.ppv, multilabel.tpr, multilabel.f1, multilabel.hamloss, multilabel.subset01)))
	print(getMultilabelBinaryPerformances(scale.pred, measures = list(acc)))
}


