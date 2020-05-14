#' Create training labels from aligned human reading database
#' @param validationHumanReading aligned human reading database
#' @param scale_type One of "location", "spatial", "temporal", default to "location"
#' @param webscrapped_trainingLabels webscrapped training labels used for comparison purpose only
#' @return training labels
make.humanReadingTrainingLabels <- function(validationHumanReading, scale_type = "location", webscrapped_trainingLabels){
	if (scale_type == "location"){
		l.df <- lapply(levels(validationHumanReading$Country.1), function(country){
			apply(validationHumanReading, MARGIN = 1, function(row) any(row == country))
		})
		trainingLabels <- do.call(cbind, l.df)
		colnames(trainingLabels) <- c("Irrelevant", levels(validationHumanReading$Country.1)[-1])
		trainingLabels <- data.frame(trainingLabels)
		trainingLabels <- trainingLabels[, which(sapply(apply(trainingLabels, 2, function(col) unique(col)), length) != 1)]
		trainingLabels <- trainingLabels[, which(apply(apply(trainingLabels, 2, function(col) table(col)), 2, min) != 1)]
		# drops <- c("Bahamas", "Barbados", "Caribbean", "Haiti", "Trinidad.and.Tobago") # countries not in database
		# trainingLabels <- trainingLabels[, which(!colnames(trainingLabels) %in% drops)]
		drops <- setdiff(colnames(trainingLabels), colnames(webscrapped_trainingLabels)) # countries not in database
		trainingLabels <- trainingLabels[, which(!colnames(trainingLabels) %in% drops)]
		missingLabels <- setdiff(colnames(webscrapped_trainingLabels), colnames(trainingLabels))
		missing_trainingLabels <- data.frame(matrix(FALSE, ncol = length(missingLabels), nrow = nrow(trainingLabels)))
		colnames(missing_trainingLabels) <- missingLabels
		print("These labels are missing from the human read labels:")
		print(missingLabels)
		trainingLabels <- cbind(trainingLabels, missing_trainingLabels)
		trainingLabels <- trainingLabels[, match(colnames(webscrapped_trainingLabels), colnames(trainingLabels))]
	} else {
		trainingLabels <- validationHumanReading
	}
	return(trainingLabels)
}

#' Create training data for a multilabel classification
#' @param validationHumanReadingDTM document-term matrix from human reading
#' @param humanReadingTrainingLabels labels from human-reading
#' @param webscrapped_validationDTM document-term matrix from webscrapping
#' @param webscrapped_trainingLabels labels from webscrapping
#' @param scale_type One of "location", "spatial", "temporal", default to "location"
#' @param aggregate_labels logical, for temporal scale, option to aggregate into three larger classes
#' @return a data.frame with nrow == nrow(validationHumanReading) + nrow(webscrapped_validationDTM) and ncol == ncol(validationHumanReadingDTM) + ncol(humanReadingTrainingLabels)
make.trainingData <- function(validationHumanReadingDTM, humanReadingTrainingLabels, webscrapped_validationDTM, webscrapped_trainingLabels, scale_type = "location", aggregate_labels = FALSE){
	humanReadingTrainingData <- cbind(validationHumanReadingDTM, humanReadingTrainingLabels)
	if (scale_type == "location"){
		humanReadingTrainingData <- cbind(validationHumanReadingDTM, humanReadingTrainingLabels)
		webscrappedTrainingData <- cbind(webscrapped_validationDTM, webscrapped_trainingLabels)
		trainingData <- rbind(humanReadingTrainingData, webscrappedTrainingData)
	} else {
		if (scale_type == "temporal" && aggregate_labels == TRUE){
			short_term <- humanReadingTrainingLabels[, c("event", "day", "week")]
			short_term <- apply(short_term, 1, function(x) any(x == TRUE))
			long_term <- humanReadingTrainingLabels[, c("years_10", "years_100")]
			long_term <- apply(long_term, 1, function(x) any(x == TRUE))
			very_long_term <- humanReadingTrainingLabels[, c("years_1000", "years_10000", "years_100000")]
			very_long_term <- apply(very_long_term, 1, function(x) any(x == TRUE))
			aggregatedHumanReadingTrainingLabels <- cbind(short_term, long_term, very_long_term)
			humanReadingTrainingData <- cbind(validationHumanReadingDTM, aggregatedHumanReadingTrainingLabels)
			humanReadingTrainingData <- as.data.frame(humanReadingTrainingData)
		}
		trainingData <- humanReadingTrainingData
	}
	return(trainingData)
}

#' internal function to get MLDR
#' @param trainingData data.frame of training data
#' @param validationHumanReadingDTM document-term matrix from human reading
#' @return MLDR a multilabel data.frame from mldr package
get.MLDR <- function(trainingData, validationHumanReadingDTM){
	MLDR <- mldr_from_dataframe(trainingData, 
		labelIndices = which(!colnames(trainingData) %in% colnames(validationHumanReadingDTM)), 
		name = "MLDR")
	return(MLDR)
}

#' Performs a simple visualization of multilabel training data using mldr package
#' @param trainingData data.frame of training data
#' @param validationHumanReadingDTM document-term matrix from human reading
#' @param humanReadingTrainingLabels labels from human-reading
EDA.trainingData <- function(trainingData, validationHumanReadingDTM, humanReadingTrainingLabels){
	MLDR <- get.MLDR(trainingData, validationHumanReadingDTM)
	layout(matrix(c(1, 2, 2, 2, 1, 2, 2, 2, 3, 4, 4, 4, 3, 4, 4, 4), 4, 4, byrow = TRUE))
	plot(MLDR, type = c("AT", "LB", "CH", "LC"), ask = FALSE, labelIndices = MLDR$labels$index)
	humanReadingTrainingData <- cbind(validationHumanReadingDTM, humanReadingTrainingLabels)
	humanReadingMLDR <- mldr_from_dataframe(
		humanReadingTrainingData, 
		labelIndices = which(colnames(humanReadingTrainingData) %in% colnames(humanReadingTrainingLabels)), 
		name = "MLDR"
		)
	comparisonDF <- cbind(country = rownames(MLDR$labels), 
		human_reading = humanReadingMLDR$labels$count, 
		human_reading_webscrapping = MLDR$labels$count
		) %>% as.data.frame() %>%
	dplyr::mutate(human_reading = as.numeric(as.character(human_reading)), 
	human_reading_webscrapping = as.numeric(as.character(human_reading_webscrapping)) 
		) %>%
	dplyr::mutate(webscrapping = human_reading_webscrapping - human_reading)
	print(comparisonDF)
}

#' Convience legacy function to create binary relance wrappers from MLR
#' @param lrn a base learner, default to "classif.svm"
#' @param trainingData data.frame of training data
#' @param validationHumanReadingDTM document-term matrix from human reading
#' @param scale_type One of "location", "spatial", "temporal", default to "location"
#' @param aggregated_labels logical
#' @return a list of learners
get.binaryRelevanceLearners <- function(lrn = "classif.svm", trainingData, validationHumanReadingDTM, scale_type = "location", aggregated_labels = FALSE){
	binary.learner <- makeLearner(lrn)
	chainingOrder <- get.chainingOrder(trainingData, validationHumanReadingDTM, scale_type = "location", aggregated_labels = FALSE)
	lrn.cc <- makeMultilabelClassifierChainsWrapper(binary.learner, order = chainingOrder)
	lrn.br <- makeMultilabelBinaryRelevanceWrapper(binary.learner)
	lrn.ns <- makeMultilabelNestedStackingWrapper(binary.learner, order = chainingOrder)
	lrn.db <- makeMultilabelDBRWrapper(binary.learner)
	lrn.st <- makeMultilabelStackingWrapper(binary.learner)
	lrns <- list(lrn.cc, lrn.br, lrn.ns, lrn.db, lrn.st)
	return(lrns)
}

#' Get chaining order from MLDR
#' @param trainingData data.frame of training data
#' @param validationHumanReadingDTM document-term matrix from human reading
#' @param scale_type One of "location", "spatial", "temporal", default to "location"
#' @param aggregated_labels logical
get.chainingOrder <- function(trainingData, validationHumanReadingDTM, scale_type = "location", aggregated_labels = FALSE){
	if (scale_type == "temporal"){
		if (aggregated_labels){
			chainingOrder <- c("very_long_term", "short_term", "long_term")
		} else {
			chainingOrder <- c("years_100000", "years_10000", "years_1000", "years_100", "day", "week", "event", "years_10", "year")
		}
	} else {
		MLDR <- get.MLDR(trainingData, validationHumanReadingDTM)
		chainingOrder <- row.names(MLDR$labels)[order(MLDR$labels$count, decreasing = TRUE)]
	}
	return(chainingOrder)
}

#' Perform a benchmark between algorithm adaptation and multilabel and binary relevance wrappers
#' @param trainingData training data
#' @param validationHumanReadingDTM training data from human reading, used to extract target column names
#' @param model_type type of predictions
#' @param scale_type One of "location", "spatial", "temporal", default to "location"
#' @param aggregated_labels logical
#' @param obs_threshold remove columns with less than this threshold
#' @return benchmark object
multilabelBenchmark <- function(trainingData, validationHumanReadingDTM, model_type, scale_type = "location", aggregated_labels = FALSE, obs_threshold = 10){
	trainingData <- trainingData[, colSums(trainingData) >= obs_threshold]
	lrns <- get.binaryRelevanceLearners(lrn = "classif.svm", trainingData, validationHumanReadingDTM, scale_type = "location", aggregated_labels = FALSE)
	lrns[[length(lrns) + 1]] <- makeLearner("multilabel.randomForestSRC", predict.type="prob")
	# lrns[[length(lrns) + 1]] <- makeLearner("multilabel.rFerns")
	set.seed(753)
	learning.task <- make.task(trainingData, validationHumanReadingDTM, model_type)
	print(learning.task)
	rdesc <- makeResampleDesc("Subsample", iters = 10, split = 3 / 4)
	bmr <- benchmark(lrns, learning.task, rdesc, 
		measures = list(multilabel.hamloss, multilabel.subset01, multilabel.acc, multilabel.tpr, multilabel.ppv, multilabel.f1), keep.pred = TRUE)
	return(bmr)

}

#' Make an MLR task
#' @param trainingData training data
#' @param validationHumanReadingDTM training data from human reading, used to extract target column names
#' @param model_type type of predictions
#' @param filter logical, if true create task for a binary classification Irrelevant/Relevant
#' @return a MLR task
make.task <- function(trainingData, validationHumanReadingDTM, model_type, filter = FALSE){
	if (model_type == "binary_relevance"){
		target <- colnames(trainingData[, which(!colnames(trainingData) %in% colnames(validationHumanReadingDTM))])
		learning.task <- makeMultilabelTask(data = trainingData, target = target)
	} else if (model_type == "label_powerset"){ # this is kept as legacy of the code to make a LP task, there's little value with our data
		MLDR <- get.MLDR(trainingData, validationHumanReadingDTM)
		MLDR.lp <- mldr_transform(MLDR, type = "LP", MLDR$labels$index)
		learning.task <- makeClassifTask(data = MLDR.lp, target = "classLabel")
	} else if (model_type == "multiclass"){
		if(filter){
			learning.task <- makeClassifTask(data = trainingData, target = "countryLabel", positive = "Relevant")
		} else {
			learning.task <- makeClassifTask(data = trainingData, target = "countryLabel")
		}
	}
	return(learning.task)
}

#' Legacy function to assess performance of learner that learned the fine temporal scale against the aggregated temporal scale
#' @param lrn learner, used to access associated benchmark predictions
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


#' Make a performance plot for multilabel classification
#' @param AggrPerformances data.frame of aggregated performance from getBMRAggrPerformances
#' @return ggplot plot
PerfVisMultilabel <- function(AggrPerformances){
	p_AggrPerformances <- AggrPerformances[, -1]
	colnames(p_AggrPerformances) <- c("learner.id", "Hamming.Loss", "Subset.0_1", "Acc", "Recall", "Precision", "F1")
	mp_AggrPerformances <- melt(p_AggrPerformances)
	ggplot(mp_AggrPerformances, aes(x = learner.id, y = value, group = learner.id, color = learner.id, fill = learner.id)) +
		geom_bar(stat="identity")+
		facet_wrap(variable ~ .) +
		cowplot::theme_cowplot() +
		labs(title = "Learner Multilabel Performance") +
		theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

#' Create training data for a multiclass classification
#' @param trainingData the already aggreated validation data 
#' @param validationHumanReadingDTM document-term matrix from human reading
#' @param humanReadingTrainingLabels labels from human-reading
#' @param webscrapped_validationDTM document-term matrix from webscrapping
#' @param webscrapped_trainingLabels labels from webscrapping
#' @param filter logical, if true create training data for a binary classification Irrelevant/Relevant
#' @return a data.frame containing the training data with a target column "countryLabelFilter" or "countryLabel"
make.trainingDataMulticlass <- function(trainingData, validationHumanReadingDTM, humanReadingTrainingLabels, webscrapped_validationDTM, webscrapped_trainingLabels, filter = FALSE, addWebscrapped = FALSE, obs_threshold = 10){
	if (filter){
		trainingData <- cbind(validationHumanReadingDTM, humanReadingTrainingLabels)
		MLDR <- get.MLDR(trainingData, validationHumanReadingDTM)
		validationDTM <- validationHumanReadingDTM
		trainingLabels <- humanReadingTrainingLabels
		trainingLabels <- trainingLabels[, order(MLDR$labels$count, decreasing = FALSE)]
		trainingLabels <- trainingLabels[, which(sort(MLDR$labels$count, decreasing = FALSE) >= obs_threshold)]
		countryLabel <- apply(trainingLabels, 1, function(row) which(row == TRUE)[1])
		validationDTM <- validationDTM[!is.na(countryLabel), ]
		validationDTM  <- as.data.frame(validationDTM)
		countryLabel <- countryLabel[!is.na(countryLabel)]
		countryLabel <- colnames(trainingLabels)[countryLabel]
		countryLabel[which(countryLabel != "Irrelevant")] <- "Relevant"
		countryLabel <- as.factor(countryLabel)
		trainingData <- cbind(validationDTM, countryLabel)
	} else {
		MLDR <- get.MLDR(trainingData, validationHumanReadingDTM)
		if (addWebscrapped){
			validationDTM <- rbind(validationHumanReadingDTM, webscrapped_validationDTM)
			trainingLabels <- rbind(humanReadingTrainingLabels, webscrapped_trainingLabels)
		} else {
			validationDTM <- validationHumanReadingDTM
			trainingLabels <- humanReadingTrainingLabels
		}
		trainingLabels <- trainingLabels[, order(MLDR$labels$count, decreasing = FALSE)]
		trainingLabels <- trainingLabels[, which(sort(MLDR$labels$count, decreasing = FALSE) >= obs_threshold)]
		countryLabel <- apply(trainingLabels, 1, function(row) which(row == TRUE)[1])
		validationDTM <- validationDTM[!is.na(countryLabel), ]
		validationDTM  <- as.data.frame(validationDTM)
		countryLabel <- countryLabel[!is.na(countryLabel)]
		countryLabel <- colnames(trainingLabels)[countryLabel]
		relevant <- which(countryLabel != "Irrelevant")
		countryLabel <- countryLabel[relevant]
		validationDTM <- validationDTM[relevant, ]
		countryLabel <- as.factor(countryLabel)
		trainingData <- cbind(validationDTM, countryLabel)
	}
	return(trainingData)
}

#' Format the data to the format expected by get_ps
#' @param data data to be formatted
#' @return list with two named elements: data and labels
format.data4get_ps <- function(data){
	.data <- list()
	.data$data <- data[, -ncol(data)]  
	.data$labels <- data[, ncol(data)]  
	return(.data)
}

#' Get the parameter set to tune to for a given learner
#' @param lrn.id learner id from MLR
#' @param data data used to construst the tuning values of the hyperparameters
#' @param grid_resolution resolution of the tuning grid
#' @return a MLR parameter set
get_ps <- function(lrn.id, data, grid_resolution){
	if (lrn.id == "classif.svm"){
		par_range <- caret::getModelInfo("svmLinear")[[1]]$grid(x = data$data, y = data$labels, len = grid_resolution)
		ps <- makeParamSet(
		  	makeDiscreteParam("cost", values = par_range$tau)		
		)
	} else if (lrn.id == "classif.randomForest"){
		par_range <- caret::getModelInfo("rf")[[1]]$grid(x = data$data, y = data$labels, len = grid_resolution)
		ps <- makeParamSet(
		  	makeDiscreteParam("mtry", values = par_range$mtry)		
		)
	} else  if(lrn.id == "classif.multinom"){
		par_range <- caret::getModelInfo("multinom")[[1]]$grid(x = data$data, y = data$labels, len = grid_resolution)
		ps <- makeParamSet(
		  	makeDiscreteParam("decay", values = par_range$decay)		
		)
	} else if (lrn.id == "classif.nnTrain"){
		ps <- makeParamSet(
	     	makeDiscreteParam("max.number.of.layers", values = seq(2,5)),
	     	makeDiscreteParam("hidden", values = list(
				a = rep(5, 5),
				b = rep(10, 5),
				c = rep(20, 5),
				d = rep(30, 5),
				e = rep(50, 5),
				f = rep(100, 5),
				g = rep(200, 5)
				)
			),
			makeDiscreteParam("activationfun", values = c("tanh")),
			makeDiscreteParam("output", values = c("softmax")),
	      	makeDiscreteParam("numepochs", values = c(20)), # changing number of epochs
	      	# makeDiscreteParam("learningrate", values = c(0.05,0.01,0.005,0.001)),
	      	makeDiscreteParam("learningrate", values = c(0.5, 0.1, 0.05, 0.01, 0.005)),
	      	makeDiscreteParam("batchsize", values = c(16, 32, 64)),
	      	# makeDiscreteParam("batchsize", values = c(nrow(data$data))),
	      	makeDiscreteParam("momentum", values = seq(0.5, 0.9, by = 0.1)),
	      	makeDiscreteParam("hidden_dropout", values = c(0, 0.1, 0.2)),
	      	makeDiscreteParam("visible_dropout", values = c(0, 0.1, 0.2))
	    )
	}
	return(ps)
}

#' Create a list of wrapped learners
#' @param learnerList a list of learners
#' @param data used to construct the value of hyperparameters
#' @param mes measures
#' @param grid_resolution resolution of the tuning grid
#' @return list of wrapped learners
get_wrappedLearnersList <- function(learnerList, data, mes, grid_resolution){
	data <- format.data4get_ps(data)
	inner <- makeResampleDesc("Holdout", split = 0.8)
	random_learners <- c("classif.nnTrain")
	wrappedLearnersList <- lapply(learnerList, function(learner){
		ps <- get_ps(learner$id, data, grid_resolution)
		if(learner$id %in% random_learners){
			ctrl <- makeTuneControlRandom(maxit = 2 * grid_resolution)
		} else {
			ctrl <- makeTuneControlGrid(resolution = grid_resolution)
		}
		makeTuneWrapper(learner, 
			inner, 
			measures = mes, 
			ps, 
			control = ctrl,
			show.info = TRUE)
	})
	return(wrappedLearnersList) 
}

#' Perform a benchmark between non-tuned algorithm adaptation and multilabel and binary relevance wrappers
#' @param trainingData training data
#' @param model_type type of predictions
#' @param filter logical, if true create task for a binary classification Irrelevant/Relevant
#' @return benchmark object
multiclassBenchmark <- function(trainingData, model_type, filter = FALSE, tune = NULL){
	learning.task <- make.task(trainingData, NULL, model_type, filter = filter)
	print(learning.task)
	set.seed(1789)
	rdesc <- makeResampleDesc("RepCV", reps = 10, folds = 10, stratify = TRUE)
	if (filter){
		mes <- list(auc, mmce, acc, ppv, tpr, fdr, timetrain)
	} else {
		mes <- list(multiclass.au1u, acc, mmce, multiclass.aunu, timetrain)
	}
	print(paste("Optimizing against:", mes[[1]]$id))
	if (is.null(tune)){
		learners <- list(
			makeLearner("classif.featureless", predict.type = "prob"),
			makeLearner("classif.randomForest", predict.type = "prob"),
			makeLearner("classif.svm", predict.type = "prob"),
			makeLearner("classif.naiveBayes", predict.type = "prob"),
			makeLearner("classif.multinom", predict.type = "prob"),
			makeLearner("classif.xgboost", predict.type =  "prob")
		)
	} else {
		learnerList <- lapply(tune, makeLearner, predict.type = "prob")
		learners <- get_wrappedLearnersList(learnerList, trainingData, mes, grid_resolution = 16)
	}	
	bmrs <- list()
	parallelStop()
	cwd_bak <- getwd()
	setwd("F:/hguillon/research")
	for (i in seq_along(learners)){
		set.seed(1789, "L'Ecuyer-CMRG")
		if (learners[[i]]$id %in% c("h2o.deeplearning", "h2o.gbm", "h2o.glm")){
			localH2o <- h2o.init(nthreads = 8, min_mem_size='10G', max_mem_size = "20G")
			h2o.removeAll() ## clean slate - just in case the cluster was already running
			h2o.no_progress()
			bmr <- benchmark(learners[[i]], learning.task, rdesc, measures = mes, models = TRUE, keep.extract = TRUE)
			h2o.shutdown(prompt = FALSE)
		} else {
			if (is.null(tune)){
				parallelStartSocket(8, level = "mlr.resample", load.balancing = TRUE)
			} else {
				parallelStartSocket(8, level = "mlr.tuneParams", load.balancing = TRUE)
			}
			clusterSetRNGStream(iseed = 1789)
			bmr <- benchmark(learners[[i]], learning.task, rdesc, measures = mes, models = TRUE, keep.extract = TRUE)
			parallelStop()
		}
		bmrs[[i]] <- bmr
	}
	bmr <- mergeBenchmarkResults(bmrs)
	setwd(cwd_bak)
	return(bmr)
}

#' Retrieve the most frequent best tuned hyper-parameters
#' Ties are broken by taking the mininum
#' @param bmr a MLR benchmark
#' @param tuning_par a tuning parameter name
get.tuningPar <- function(bmr, tuning_par){
	getBMRTuneResults(bmr, as.df = TRUE) %>% 
	pull(tuning_par) %>% 
	na.omit() %>% 
	modeest::mfv() %>% 
	min()
}



#' Make randomForest prediction
#' @param lrn a learner name
#' @param parvals a list of best tuned hyperparameters
#' @param trainingData data to train the model
#' @param targetData data used to predict
#' @param model_type 
#' @param filter logical, if true will train and predict for the relevance filter
#' @return prediction or membership
make.predictions <- function(lrn, parvals, trainingData, targetData, model_type, filter = TRUE){
	learning.task <- make.task(trainingData, NULL, model_type, filter = filter)
	lrn.rf <- makeLearner(lrn, predict.type="prob")
	lrn.rf$par.vals <- parvals
	model <-  train(lrn.rf, learning.task)
	print(model)
	predTarget <- predict(model, newdata = targetData)
	if (filter){
		return(predTarget$data$response)
	} else {
		return(predTarget$data)
	}
}

#' Create the target data to predict from
#' @param DTM the complete document-term matrix
#' @return a data.frame
make.targetData <- function(DTM){
	targetData <- as.matrix(DTM)
	colnames(targetData) <- paste0("Term", seq(ncol(DTM)))
	targetData <- data.frame(targetData)
	return(targetData)
}

#' Create a comparison plot of AUC
#' @param bmr benchmark results from MLR
#' @param binary logical, if TRUE it's a binary classification and the measure name has to be changed
#' @return named list: p_auc: the plot; medians: a table with median value of auc
make.AUCPlot <- function(bmr, binary = FALSE){
	perf <- getBMRPerformances(bmr, as.df = TRUE) %>% 
		mutate(learner.id = gsub("classif.", "", learner.id)) %>% 
		filter(learner.id != "featureless")
	colnames(perf) <- gsub(".test.mean", "", colnames(perf))
	if (binary) perf <- perf %>% rename(multiclass.au1u = auc)
	medians <- perf %>% group_by(learner.id) %>% summarize(median = median(multiclass.au1u)) %>% arrange(-median)
	perf <- perf %>% mutate(learner.id = forcats::fct_reorder(learner.id, multiclass.au1u, median, .desc = TRUE))
	stat.test <- perf %>% t_test(multiclass.au1u ~ learner.id, var.equal = FALSE, p.adjust.method = "none") %>%
		adjust_pvalue() %>% add_significance()
	p_auc <- ggviolin(perf, 
		x = "learner.id", 
		y = "multiclass.au1u",
		add = c("jitter", "median"),
		add.params = list("alpha" = .5)) +
		stat_pvalue_manual(
			stat.test %>% mutate(y.position = 1.05),  
			label = "p.adj.signif",
			step.increase = 0.1, 
			hide.ns = TRUE) +
		labs(x = "ML Model", 
			y = "Multiclass 1v1 AUC") + 
		ggpubr::theme_pubr()
	return(list(p_auc = p_auc, medians = medians))
}


transform.DTM <- function(DTM, change.col = TRUE){
 DTM <- DTM %>% as.matrix() %>% as.data.frame() %>%
	mutate(antigua = antigua + barbuda) %>% select(- barbuda) %>% rename(Antigua.and.Barbuda = antigua) %>%
	mutate(costa = costa + rica) %>% select(-rica) %>% rename(Costa.Rica = costa) %>%
	mutate(dominican = dominican + republ) %>% select(- republ) %>% rename(Dominican.Republic = dominican) %>%
	mutate(kitt = kitt + nevi) %>% select(- nevi) %>% rename(St..Kitts.and.Nevis = kitt) %>%
	mutate(vincent = vincent + grenadin) %>% select(- grenadin) %>% rename(St..Vincent.and.the.Grenadines = vincent) %>%
	mutate(trinidad = trinidad + tobago) %>% select(- tobago) %>% rename(Trinidad.and.Tobago = trinidad) %>%
	rename(Argentina = argentina, 
		Bahamas = bahama,
		Barbados = barbado,
		Belize = beliz,
		Brazil = brazil,
		Bolivia = bolivia,
		Chile = chile,
		Colombia = colombia,
		Cuba = cuba,
		Dominica = dominica,
		Ecuador = ecuador,
		El.Salvador = salvador,
		Grenada = grenada,
		Guatemala = guatemala,
		Guyana = guyana,
		Haiti = haiti,
		Honduras = hondura,
		Jamaica = jamaica,
		Mexico = mexico,
		Nicaragua = nicaragua,
		Panama = panama,
		Paraguay = paraguay,
		Peru = peru,
		St..Lucia = lucia,
		Suriname = surinam,
		Uruguay = uruguay,
		Venezuela = venezuela
		)
	if(change.col) colnames(DTM) <- paste0("Term", seq(ncol(DTM)))
	return(DTM)
}	