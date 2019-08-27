leg_validationDTM <- validationDTM
leg_trainingLabels <- trainingLabels

validationDTM <- rbind(leg_validationDTM, webscrapped_validationDTM)
trainingLabels <- rbind(leg_trainingLabels, webscrapped_trainingLabels)

trainingData <- cbind(validationDTM, trainingLabels)
MLDR <- mldr_from_dataframe(trainingData, 
	labelIndices = which(colnames(trainingData) %in% colnames(trainingLabels)), 
	name = "MLDR")

chainingOrder <- row.names(MLDR$labels)[order(MLDR$labels$count, decreasing = TRUE)]	

trainingLabels <- trainingLabels[, order(MLDR$labels$count)]

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
lrns <- list(lrn.rf, lrn.svm)
rdesc <- makeResampleDesc("Subsample", iters = 3, split = 3 / 4)
bmr <- benchmark(lrns, scale.task, rdesc, measures = list(acc, multiclass.au1u, multiclass.aunu), keep.pred = TRUE)

library("ggplot2")
library("h2o")
library("forcats")
library("dplyr")
library("cowplot")
h2o.init(nthreads = 6, min_mem_size='10G', max_mem_size = "20G")
h2o.no_progress()


trainh2o <- as.h2o(trainingData)
y <- "countryLabel"
x <- setdiff(colnames(trainh2o),y)


hyper_params <- list( 
	activation = c("Rectifier", "Maxout", "Tanh", "RectifierWithDropout", "MaxoutWithDropout", "TanhWithDropout"), 
	# activation = c("Rectifier", "Maxout", "Tanh"), 
	hidden = list(c(20, 20), c(50, 50), c(5, 5, 5, 5, 5), c(10, 10, 10, 10), c(50, 50, 50), c(100, 100, 100)),
	l1 = seq(0, 1e-4, length.out = 20),
	l2 = seq(0, 1e-4, length.out = 20),
	rate = c(0, 01, 0.005, 0.001),
	rate_annealing = c(1e-8, 1e-7, 1e-6),
	rho = c(0.9, 0.95, 0.99, 0.999),
	epsilon = c(1e-10, 1e-8, 1e-6, 1e-4),
	momentum_start = c(0, 0.5),
	momentum_stable = c(0.99, 0.5, 0),
	input_dropout_ratio = c(0, 0.1, 0.2),
	max_w2 = c(10, 100, 1000, 3.4028235e+38)
	)

search_criteria <- list(
	strategy = "RandomDiscrete", 
	max_runtime_secs = 3600 * 2,
	stopping_metric = "misclassification",
	stopping_tolerance = 0.01,
	stopping_rounds = 15,
	seed = -1)

deepgrid <- h2o.grid(
		x = x,
		y = y,
		algorithm = "deeplearning",
		training_frame = trainh2o,
		standardize = FALSE,
		ignore_const_cols = FALSE,
		nfolds = 10,
		fold_assignment = "Stratified",
		epochs = 20,
		variable_importances = TRUE,
		hyper_params = hyper_params,
		search_criteria = search_criteria,
		seed = -1)

dl_gridperf_acc <- h2o.getGrid(deepgrid@grid_id, sort_by = "accuracy", decreasing = TRUE)
head(dl_gridperf_acc@summary_table, 20)