target <- colnames(trainingLabels)

dim(trainingData)

set.seed(1789)
scale.task <- makeMultilabelTask(data = trainingData, target = target)
scale.task

# n <-  getTaskSize(scale.task)
# train.set <-  sample(seq(n), n * .8)
# test.set <-  which(!seq(n) %in% train.set)