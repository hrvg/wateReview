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


#' Performs a simple visualization of multilabel training data using mldr package
#' @param trainingData data.frame of training data
#' @param validationHumanReadingDTM document-term matrix from human reading
#' @param humanReadingTrainingLabels labels from human-reading
EDA.trainingData <- function(trainingData, validationHumanReadingDTM, humanReadingTrainingLabels){
	MLDR <- mldr_from_dataframe(trainingData, 
		labelIndices = which(!colnames(trainingData) %in% colnames(validationHumanReadingDTM)), 
		name = "MLDR")
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
