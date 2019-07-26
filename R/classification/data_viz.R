# bind the validation data to the topicDocs
# trainingData <- cbind(validationDTM, trainingLabels)

trainingData <- cbind(validationDTM, trainingLabels)
# trainingData <- cbind(validationTopicDocs, trainingLabels)
# trainingData <- cbind(validationTopicDocs, validationDTM, trainingLabels)

spatial_scale_MLDR <- mldr_from_dataframe(trainingData, 
	labelIndices = which(colnames(trainingData) %in% colnames(trainingLabels)), 
	name = "spatial_scale_MLDR")

plot(spatial_scale_MLDR, type = c("LC"), labelIndices = spatial_scale_MLDR$labels$index)
plot(spatial_scale_MLDR, type = c("LH"))
plot(spatial_scale_MLDR, type = c("LSH"))
plot(spatial_scale_MLDR, type = c("LB"))
plot(spatial_scale_MLDR, type = c("LSB"))
plot(spatial_scale_MLDR, type = c("CH"))
plot(spatial_scale_MLDR, type = c("AT"))

# if (scale_type == "temporal") trainingLabels <- trainingLabels[, -4]
# trainingData <- cbind(validationTopicDocs, trainingLabels)