trainingData <- cbind(validationDTM, trainingLabels)
# trainingData <- cbind(validationTopicDocs, trainingLabels)
# trainingData <- cbind(validationTopicDocs, validationDTM, trainingLabels)

dim(trainingData)

spatial_scale_MLDR <- mldr_from_dataframe(trainingData, 
	labelIndices = which(colnames(trainingData) %in% colnames(trainingLabels)), 
	name = "spatial_scale_MLDR")
spatial_scale_MLDR.lp <- mldr_transform(spatial_scale_MLDR, type = "LP", spatial_scale_MLDR$labels$index)
lbls <- as.character(spatial_scale_MLDR.lp$classLabel)
lbls <- gsub("TRUEFALSE", "short_term", lbls)
lbls <- gsub("TRUETRUE", "noise", lbls)
lbls <- gsub("FALSETRUE", "long_term", lbls)
lbls <- gsub("FALSEFALSE", "noise", lbls)
spatial_scale_MLDR.lp$classLabel <- as.factor(lbls)

library("ggplot2")
library("h2o")
library("forcats")
library("dplyr")
library("cowplot")

h2o.shutdown()
localH2o <- h2o.init(nthreads = 6, min_mem_size='10G', max_mem_size = "20G")
h2o.removeAll() 


trainh2o <- as.h2o(spatial_scale_MLDR.lp)
testh2o <- trainh2o[, -ncol(trainh2o)]
y <- "classLabel"
x <- setdiff(colnames(trainh2o),y)


dl_model <-	h2o.deeplearning(x = x,
	y = y,
	training_frame = trainh2o,
	nfolds = 5,
	epochs = 1e3)


################################################################################
########################## INITIAL RANDOM GRID SEARCH ##########################
################################################################################


hyper_params <- list( 
	activation = c("Rectifier", "Maxout", "Tanh", "RectifierWithDropout", "MaxoutWithDropout", "TanhWithDropout"), 
	# activation = c("Rectifier", "Maxout", "Tanh"), 
	hidden = list(
		c((ncol(trainh2o) - 1) %/% 2),
		c((ncol(trainh2o) - 1) %/% 2, (ncol(trainh2o) - 1) %/% 2),
		c((ncol(trainh2o) - 1) %/% 4, (ncol(trainh2o) - 1) %/% 4),
		c((ncol(trainh2o) - 1) %/% 8, (ncol(trainh2o) - 1) %/% 8),
		c((ncol(trainh2o) - 1) %/% 16, (ncol(trainh2o) - 1) %/% 16),
		c((ncol(trainh2o) - 1) %/% 2, (ncol(trainh2o) - 1) %/% 4, (ncol(trainh2o) - 1) %/% 8, (ncol(trainh2o) - 1) %/% 16),
		c((ncol(trainh2o) - 1) %/% 2, (ncol(trainh2o) - 1) %/% 4, (ncol(trainh2o) - 1) %/% 2),
		c((ncol(trainh2o) - 1) %/% 2, (ncol(trainh2o) - 1) %/% 4, (ncol(trainh2o) - 1) %/% 8, (ncol(trainh2o) - 1) %/% 4, (ncol(trainh2o) - 1) %/% 2),
		c((ncol(trainh2o) - 1) %/% 2, (ncol(trainh2o) - 1) %/% 4, (ncol(trainh2o) - 1) %/% 8, (ncol(trainh2o) - 1) %/% 16, (ncol(trainh2o) - 1) %/% 8, (ncol(trainh2o) - 1) %/% 4, (ncol(trainh2o) - 1) %/% 2)
		),
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
	max_runtime_secs = 120,
	stopping_metric = "misclassification",
	stopping_tolerance = 0.01,
	stopping_rounds = 15,
	seed = -1)

deepgrid <- h2o.grid(
		x = x,
		y = y,
		algorithm = "deeplearning",
		training_frame = trainh2o,
		standardize = TRUE,
		ignore_const_cols = TRUE,
		nfolds = 10,
		fold_assignment = "Stratified",
		epochs = 20,
		variable_importances = FALSE,
		hyper_params = hyper_params,
		search_criteria = search_criteria,
		seed = -1)

dl_gridperf_acc <- h2o.getGrid(deepgrid@grid_id, sort_by = "accuracy", decreasing = TRUE)
head(dl_gridperf_acc@summary_table, 20)