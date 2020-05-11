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
