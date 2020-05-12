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

#' Make training data
#' @param validationHumanReadingDTM document-term matrix from human reading
#' @param humanReadingTrainingLabels labels from human-reading
#' @param webscrapped_validationDTM document-term matrix from webscrapping
#' @param webscrapped_trainingLabels labels from webscrapping
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
#' @return list with multiple elements, one of them being an MLR task
make.task <- function(trainingData, validationHumanReadingDTM, model_type){
	if (model_type == "binary_relevance"){
		target <- colnames(trainingData[, which(!colnames(trainingData) %in% colnames(validationHumanReadingDTM))])
		learning.task <- makeMultilabelTask(data = trainingData, target = target)
	} else if (model_type == "label_powerset"){ # this is kept as legacy of the code to make a LP task, there's little value with our data
		MLDR <- get.MLDR(trainingData, validationHumanReadingDTM)
		MLDR.lp <- mldr_transform(MLDR, type = "LP", MLDR$labels$index)
		learning.task <- makeClassifTask(data = MLDR.lp, target = "classLabel")
	} else if (model_type == "multiclass"){
		learning.task <- NULL
	}
	return(learning.task)
}

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
