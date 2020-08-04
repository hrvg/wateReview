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
	get_DocTermMatrix,
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
	transform.DTM,
	make.AUCPlot,
	get.tuningPar
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
DTM <- get_DocTermMatrix()
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


cwd_bak <- getwd()
setwd("F:/hguillon/research")

# multilabel: binary relevance and algorithm adaptation

# bmr <- multilabelBenchmark(trainingData, validationHumanReadingDTM, MODEL_TYPE, scale_type = SCALE_TYPE, aggregated_labels = AGGREGATE, obs_threshold = 10)
# AggrPerformances <- getBMRAggrPerformances(bmr, as.df = TRUE)
# PerfVisMultilabel(AggrPerformances)

# multiclass

# ## relevance filter
trainingDataMulticlassFilter <- make.trainingDataMulticlass(trainingData, validationHumanReadingDTM, humanReadingTrainingLabels, webscrapped_validationDTM, webscrapped_trainingLabels, 
	filter = TRUE, 
	addTopicDocs = TRUE, 
	validationTopicDocs = validationTopicDocs)

# ### selecting a model to tune
if (!file.exists("bmr_filter.Rds")){
	bmr_filter <- multiclassBenchmark(trainingDataMulticlassFilter, MODEL_TYPE, filter = TRUE)
	saveRDS(bmr_filter, "bmr_filter.Rds")
} else {
	bmr_filter <- readRDS("bmr_filter.Rds")
}
make.AUCPlot(bmr_filter, binary = TRUE)
# based on the result of the AUC plot comparison, svm, multinom and RF are selected for tuning benchmark

# ### tuning model

if(!file.exists("bmr_tune_filter.Rds")){
	bmr_tune_filter <- multiclassBenchmark(trainingDataMulticlassFilter, MODEL_TYPE, filter = TRUE, tune = list("classif.svm", "classif.randomForest", "classif.multinom"))
	saveRDS(bmr_tune_filter, "bmr_tune_filter.Rds")
} else {
	bmr_tune_filter <- readRDS("bmr_tune_filter.Rds")
}
make.AUCPlot(bmr_tune_filter, binary = TRUE)
# RF and multninomal have similar performance, we pick multinom (better hyper-parameter constrains)

# ### predict
targetData <- make.targetData(DTM, addTopicDocs = TRUE, topicDocs = topicDocs)
predRelevance <- make.predictions("classif.multinom", 
	list(mtry = get.tuningPar(bmr_tune_filter, "decay")),
	trainingDataMulticlassFilter, targetData, MODEL_TYPE, filter = TRUE)
table(predRelevance)

## country
trainingDataMulticlass <- make.trainingDataMulticlass(trainingData, validationHumanReadingDTM, humanReadingTrainingLabels, webscrapped_validationDTM, webscrapped_trainingLabels, filter = FALSE, addWebscrapped = TRUE)

### selecting a model to tune
if (!file.exists("bmr_country.Rds")){
	bmr_country <- multiclassBenchmark(trainingDataMulticlass, MODEL_TYPE, filter = FALSE)
	saveRDS(bmr_country, "bmr_country.Rds")
} else {
	bmr_country <- readRDS("bmr_country.Rds")
}
make.AUCPlot(bmr_country)

# bmr_tune_country <- multiclassBenchmark(trainingDataMulticlass, MODEL_TYPE, filter = FALSE, tune = list("classif.randomForest"))
# make.AUCPlot(bmr_tune_country)
# saveRDS(bmr_tune_country, "bmr_tune_country.Rds")

# ### predict
targetData <- make.targetData(DTM)
predCountry <- make.predictions("classif.randomForest", 
	list(mtry = floor(sqrt(ncol(trainingDataMulticlass) - 1))),
	trainingDataMulticlass, targetData, MODEL_TYPE, filter = FALSE)

predCountry$response <- as.character(predCountry$response)
predCountry$response[as.character(predRelevance) == "Irrelevant"] <- "Irrelevant"
predCountry$response <- as.factor(predCountry$response)

setwd(cwd_bak)

saveRDS(predRelevance, "predRelevance.Rds")
saveRDS(predCountry, "predCountry.Rds")