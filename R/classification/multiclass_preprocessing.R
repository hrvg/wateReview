leg_validationDTM <- validationDTM
leg_trainingLabels <- trainingLabels

validationDTM <- rbind(leg_validationDTM, webscrapped_validationDTM)
trainingLabels <- rbind(leg_trainingLabels, webscrapped_trainingLabels)

trainingData <- cbind(validationDTM, trainingLabels)
MLDR <- mldr_from_dataframe(trainingData, 
	labelIndices = which(colnames(trainingData) %in% colnames(trainingLabels)), 
	name = "MLDR")

chainingOrder <- row.names(MLDR$labels)[order(MLDR$labels$count, decreasing = TRUE)]	

trainingLabels <- trainingLabels[, order(MLDR$labels$count, decreasing = FALSE)]
trainingLabels <- trainingLabels[, which(sort(MLDR$labels$count, decreasing = FALSE) >= 10)]

countryLabel <- apply(trainingLabels, 1, function(row) which(row == TRUE)[1])

validationDTM <- validationDTM[!is.na(countryLabel), ]
validationDTM  <- as.data.frame(validationDTM)
countryLabel <- countryLabel[!is.na(countryLabel)]
countryLabel <- colnames(trainingLabels)[countryLabel]
countryLabel <- as.factor(countryLabel)

trainingData <- cbind(validationDTM, countryLabel)
target <- "countryLabel"