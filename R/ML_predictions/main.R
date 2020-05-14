### libraries
library("mlr")
library("parallelMap")
library("parallel")
library("OpenML")
library("NLP")
library("tm")
library("data.table")
library("mldr")
library("reshape2")
library("ggplot2")
library("rstatix")
library("ggpubr")
library("dplyr")


### utils ###
import::here(.from = "./R/utils/lib_shared.R", 
	get_titleInd,
	get_validationHumanReading,
	get_topicDocs,
	get_titleDocs,
	get_webscrapped_validationDTM,
	get_webscrapped_trainingLabels,
	get_DTM,
	align.humanReadingTopicModel,
	QA.alignedData
)

import::here(.from = "./R/utils/lib_ML_predictions.R", 
	make.humanReadingTrainingLabels,
	make.trainingData,
	get.MLDR,
	EDA.trainingData,
	get.binaryRelevanceLearners,
	get.chainingOrder,
	multilabelBenchmark,
	make.task,
	get_short_long_term_pred,
	PerfVisMultilabel,
	make.trainingDataMulticlass,
	multiclassBenchmark,
	make.predictions,
	make.targetData,
	transform.DTM
)

### main

# param
SCALE_TYPE <- "location"
MODEL_TYPE <- "multiclass" # multiclass or binary_relevance
AGGREGATE <- FALSE

# data reading
topicDocs <- get_topicDocs()
titleDocs <- get_titleDocs(topicDocs)
validationHumanReading <-  get_validationHumanReading(scale_type = SCALE_TYPE)
DTM <- get_DTM()
webscrapped_validationDTM <- get_webscrapped_validationDTM()
colnames(webscrapped_validationDTM) <- colnames(DTM)
webscrapped_validationDTM <- transform.DTM(webscrapped_validationDTM)
DTM <- transform.DTM(DTM)
webscrapped_trainingLabels <- get_webscrapped_trainingLabels()
titleInd <- get_titleInd(humanReadingDatabase = validationHumanReading, topicModelTitles = titleDocs)

# sanity check
# check if all the papers are found, address issues, align databases
table(is.na(titleInd))
alignedData <- align.humanReadingTopicModel(titleInd, validationHumanReading, topicDocs, DTM)
alignedData <- QA.alignedData(alignedData, scale_type = SCALE_TYPE)
validationTopicDocs <- alignedData$validationTopicDocs
validationHumanReading <- alignedData$validationHumanReading
validationHumanReadingDTM <- alignedData$validationDTM

# get humanReadingTrainingLabels
humanReadingTrainingLabels <- make.humanReadingTrainingLabels(validationHumanReading, scale_type = SCALE_TYPE, webscrapped_trainingLabels)
trainingData <- make.trainingData(validationHumanReadingDTM, humanReadingTrainingLabels, webscrapped_validationDTM, webscrapped_trainingLabels, scale_type = SCALE_TYPE, aggregate_labels = AGGREGATE)

# exploratory data analysis
EDA.trainingData(trainingData, validationHumanReadingDTM, humanReadingTrainingLabels)

# multilabel: binary relevance and algorithm adaptation

# bmr <- multilabelBenchmark(trainingData, validationHumanReadingDTM, MODEL_TYPE, scale_type = SCALE_TYPE, aggregated_labels = AGGREGATE, obs_threshold = 10)
# AggrPerformances <- getBMRAggrPerformances(bmr, as.df = TRUE)
# PerfVisMultilabel(AggrPerformances)

# multiclass

# ## relevance filter
trainingDataMulticlassFilter <- make.trainingDataMulticlass(trainingData, validationHumanReadingDTM, humanReadingTrainingLabels, webscrapped_validationDTM, webscrapped_trainingLabels, filter = TRUE)

# ### selecting a model to tune
bmr_filter <- multiclassBenchmark(trainingDataMulticlassFilter, MODEL_TYPE, filter = TRUE)
saveRDS(bmr_filter, "bmr_filter.Rds")
print(bmr_filter)
make.AUCPlot(bmr_filter, binary = TRUE)
# based on the result of the AUC plot comparison, svm and RF are selected for tuning benchmark

# ### tuning model
bmr_tune_filter <- multiclassBenchmark(trainingDataMulticlassFilter, MODEL_TYPE, filter = TRUE, tune = list("classif.svm", "classif.randomForest"))
saveRDS(bmr_tune_filter, "bmr_tune_filter.Rds")
make.AUCPlot(bmr_tune_filter, binary = TRUE)
# RF and SVM have similar performance, we pick RF

# ### predict
targetData <- make.targetData(DTM)
predRelevance <- make.predictions("classif.randomForest", 
	list(mtry = get.tuningPar(bmr_tune_filter, "mtry")),
	trainingDataMulticlassFilter, targetData, MODEL_TYPE, filter = TRUE)

## country
trainingDataMulticlass <- make.trainingDataMulticlass(trainingData, validationHumanReadingDTM, humanReadingTrainingLabels, webscrapped_validationDTM, webscrapped_trainingLabels, filter = FALSE, addWebscrapped = TRUE)

### selecting a model to tune
bmr_country <- multiclassBenchmark(trainingDataMulticlass, MODEL_TYPE, filter = FALSE)
print(bmr_country)
make.AUCPlot(bmr_country)
saveRDS(bmr_country, "bmr_country.Rds")

bmr_tune_country <- multiclassBenchmark(trainingDataMulticlass, MODEL_TYPE, filter = FALSE, tune = list("classif.multinom"))
saveRDS(bmr_tune_country, "bmr_tune_country.Rds")

# ### predict
targetData <- make.targetData(DTM)
predCountry <- make.predictions("classif.multinom", 
	list(decay = get.tuningPar(bmr_tune_filter, "decay")),
	trainingDataMulticlassFilter, targetData, MODEL_TYPE, filter = FALSE)


## predictions
predCountry_new <- as.character(predCountry$response)
predCountry_new[which(as.character(predRelevance) == "Irrelevant")] <- "Irrelevant"
predCountry_old <- as.character(readRDS("./predCountry.Rds"))
table(predCountry_old == predCountry_new[-missing]) / sum(table(predCountry_old == predCountry_new[-missing]) )


predHumanWebscrapped <- trainingDataMulticlass[, ncol(trainingDataMulticlass)]
testDTM <- trainingDataMulticlass[, -ncol(trainingDataMulticlass)]
colnames(testDTM) <- colnames(get_DTM() %>% transform.DTM(change.col = FALSE))
testDTM <- sweep(testDTM, 1, 1, "+")
testDTM <- sweep(testDTM, 1, rowSums(testDTM), "/")

predMining <- apply(testDTM, 1, function(row) colnames(testDTM)[which.max(row)]) %>% unname() %>% unlist()

table(predHumanWebscrapped == predMining) / length(predMining)

print("False Negative")
table(factor(predHumanWebscrapped[which(predHumanWebscrapped != predMining)], levels = unique(predHumanWebscrapped)))

print("False Negative Rate")
signif(table(factor(predHumanWebscrapped[which(predHumanWebscrapped != predMining)], levels = unique(predHumanWebscrapped))) /
table(factor(predHumanWebscrapped, levels = unique(predHumanWebscrapped))), digits = 2)

print("True Positive")
table(factor(predHumanWebscrapped[which(predHumanWebscrapped == predMining)], levels = unique(predHumanWebscrapped)))

print("True Positive Rate")
signif(table(factor(predHumanWebscrapped[which(predHumanWebscrapped == predMining)], levels = unique(predHumanWebscrapped))) /
table(factor(predHumanWebscrapped, levels = unique(predHumanWebscrapped))), digits = 2)