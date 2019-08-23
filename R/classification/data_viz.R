# bind the validation data to the topicDocs
# trainingData <- cbind(validationDTM, trainingLabels)

trainingData <- cbind(validationDTM, trainingLabels)
# trainingData <- cbind(validationTopicDocs, trainingLabels)
# trainingData <- cbind(validationTopicDocs, validationDTM, trainingLabels)

MLDR <- mldr_from_dataframe(trainingData, 
	labelIndices = which(colnames(trainingData) %in% colnames(trainingLabels)), 
	name = "MLDR")

plot(MLDR, type = c("LC"), labelIndices = MLDR$labels$index)
plot(MLDR, type = c("LH"))
plot(MLDR, type = c("LSH"))
plot(MLDR, type = c("LB"))
plot(MLDR, type = c("LSB"))
plot(MLDR, type = c("CH"))
plot(MLDR, type = c("AT"))

# if (scale_type == "temporal") trainingLabels <- trainingLabels[, -4]
# trainingData <- cbind(validationTopicDocs, trainingLabels)