lrn.rfsrc <- makeLearner("multilabel.randomForestSRC")
lrn.rFerns <- makeLearner("multilabel.rFerns")

lrns <- list(lrn.rfsrc, lrn.rFerns)

for (lrn in lrns){
	scale.mod <- train(lrn, scale.task, subset = train.set)
	scale.pred <- predict(scale.mod, task = scale.task, subset = test.set)
	print(performance(scale.pred, measures = list(multilabel.acc, multilabel.ppv, multilabel.tpr, multilabel.f1, multilabel.hamloss, multilabel.subset01)))
	print(getMultilabelBinaryPerformances(scale.pred, measures = list(acc)))
}
