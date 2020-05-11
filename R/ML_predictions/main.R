### libraries
library("mlr")
library("OpenML")
library("NLP")
library("tm")
library("data.table")
library("mldr")

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
	EDA.trainingData
)

### main

# param
SCALE_TYPE <- "location"
MODEL_TYPE <- "multiclass" # one of multiclass, binary_relevance, label_powerset
AGGREGATE <- FALSE

# data reading
topicDocs <- get_topicDocs()
titleDocs <- get_titleDocs(topicDocs)
validationHumanReading <-  get_validationHumanReading(scale_type = SCALE_TYPE)
webscrapped_validationDTM <- get_webscrapped_validationDTM()
webscrapped_trainingLabels <- get_webscrapped_trainingLabels()
titleInd <- get_titleInd(humanReadingDatabase = validationHumanReading, topicModelTitles = titleDocs)
DTM <- get_DTM()

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