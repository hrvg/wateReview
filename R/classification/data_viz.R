# bind the validation data to the topicDocs
# trainingData <- cbind(validationDTM, trainingLabels)

leg_validationDTM <- validationDTM
leg_trainingLabels <- trainingLabels

validationDTM <- rbind(leg_validationDTM, webscrapped_validationDTM)
trainingLabels <- rbind(leg_trainingLabels, webscrapped_trainingLabels)

trainingData <- cbind(validationDTM, trainingLabels)
leg_trainingData <- cbind(leg_validationDTM, leg_trainingLabels)

# trainingData <- cbind(validationTopicDocs, trainingLabels)
# trainingData <- cbind(validationTopicDocs, validationDTM, trainingLabels)

MLDR <- mldr_from_dataframe(trainingData, 
	labelIndices = which(colnames(trainingData) %in% colnames(trainingLabels)), 
	name = "MLDR")


leg_MLDR <- mldr_from_dataframe(leg_trainingData, 
	labelIndices = which(colnames(leg_trainingData) %in% colnames(leg_trainingLabels)), 
	name = "legacy MLDR")

plot(MLDR, type = c("LC"), labelIndices = MLDR$labels$index)
plot(MLDR, type = c("LH"))
plot(MLDR, type = c("LSH"))
plot(MLDR, type = c("LB"))
plot(MLDR, type = c("LSB"))
plot(MLDR, type = c("CH"))
plot(MLDR, type = c("AT"))

# if (scale_type == "temporal") trainingLabels <- trainingLabels[, -4]
# trainingData <- cbind(validationTopicDocs, trainingLabels)
cbind(country = rownames(MLDR$labels), human_reading = leg_MLDR$labels$count, human_and_webscrapping = MLDR$labels$count)