# emotionsbr <- mldr_transform(emotions, type = "BR")
spatial_scale_MLDR.lp <- mldr_transform(spatial_scale_MLDR, type = "LP", spatial_scale_MLDR$labels$index)

set.seed(1789)
scale.task <- makeClassifTask(data = spatial_scale_MLDR.lp, target = "classLabel")
# scale.task = makeMultilabelTask(data = validationHumanReadingTopicDocs, target = target)

scale.task

n <-  getTaskSize(scale.task)
train.set <-  sample(seq(n), n * .8)
test.set <-  which(!seq(n) %in% train.set)

lrn <- makeLearner("classif.randomForest")

scale.mod <- train(lrn, scale.task, subset = train.set)
scale.pred <- predict(scale.mod, task = scale.task, subset = test.set)
print(performance(scale.pred, measures = list(acc, mmce, kappa)))
