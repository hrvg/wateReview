validationDTMFilter <- leg_validationDTM
# validationDTMFilter <- cbind(leg_validationDTM, validationTopicDocs)
trainingLabelsFilter <- leg_trainingLabels

trainingDataFilter <- cbind(validationDTMFilter, trainingLabelsFilter)
MLDRFilter <- mldr_from_dataframe(trainingDataFilter, 
	labelIndices = which(colnames(trainingDataFilter) %in% colnames(trainingLabelsFilter)), 
	name = "MLDR")

trainingLabelsFilter <- trainingLabelsFilter[, order(MLDRFilter$labels$count, decreasing = FALSE)]
trainingLabelsFilter <- trainingLabelsFilter[, which(sort(MLDRFilter$labels$count, decreasing = FALSE) >= 10)]

countryLabelFilter <- apply(trainingLabelsFilter, 1, function(row) which(row == TRUE)[1])

validationDTMFilter <- validationDTMFilter[!is.na(countryLabelFilter), ]
validationDTMFilter  <- as.data.frame(validationDTMFilter)
countryLabelFilter <- countryLabelFilter[!is.na(countryLabelFilter)]
countryLabelFilter <- colnames(trainingLabelsFilter)[countryLabelFilter]
countryLabelFilter[which(countryLabelFilter != "Irrelevant")] <- "Relevant"
countryLabelFilter <- as.factor(countryLabelFilter)

trainingDataFilter <- cbind(validationDTMFilter, countryLabelFilter)
targetFilter <- "countryLabelFilter"