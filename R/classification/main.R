# libraries
library(mlr)
library(OpenML)
library(NLP)
library(tm)
library(data.table)
library(mldr)
# main

# read data
topicDocs <- readRDS("F:/hguillon/research/exploitation/R/latin_america/data/topicDocs.Rds")
titleDocs <- readLines("F:/hguillon/research/exploitation/R/latin_america/data/info.dat")
if (nrow(topicDocs) != length(titleDocs)) warning("Dimensions not matching")

# scale_type <- "spatial"
# scale_type <- "temporal"
scale_type <- "location"

validationHumanReading <- read.csv(paste0("F:/hguillon/research/exploitation/R/latin_america/data/validation_df_", scale_type, ".csv"))
validationHumanReading <- validationHumanReading[validationHumanReading$title != "", ]

# get DTM
dtm_file <- "F:/hguillon/research/exploitation/R/latin_america/data/obj_dtm.Rds"
dtm_file <- "F:/hguillon/research/exploitation/R/latin_america/data/obj_dtm_abstract.Rds"
if(!file.exists(dtm_file)){
	lines <- readLines("F:/hguillon/research/exploitation/R/latin_america/data/corpus.dat")
	vec <- VectorSource(lines)
	obj_corpus <- Corpus(vec)
	obj_dtm <- DocumentTermMatrix(obj_corpus)
	saveRDS <- saveRDS(obj_dtm, dtm_file)
}
obj_dtm <- readRDS(dtm_file)
obj_dtm <- removeSparseTerms(obj_dtm, 0.99)

titleInd_file <- "F:/hguillon/research/exploitation/R/latin_america/data/titleInd.Rds"
if(!file.exists(titleInd_file)){
	# modify the format of titles so that match between titleDocs and validationHumanReading
	titleValidation <- as.character(validationHumanReading$title)
	titleValidation <- gsub(".pdf", "", titleValidation)
	titleValidation <- gsub(" ", "_", titleValidation)
	titleValidation <- gsub("[^\x20-\x7E]", "", titleValidation)
	titleValidation <- gsub("\\(", "", titleValidation)
	titleValidation <- gsub("\\)", "", titleValidation)
	titleValidation <- gsub("\\'", "", titleValidation)
	titleValidation <- gsub(",", "", titleValidation)
	# look for matches
	titleInd <- sapply(titleValidation, function(t) grep(t, titleDocs)[1])
	saveRDS(titleInd, titleInd_file)
}

titleInd <- readRDS(titleInd_file)

# check that all the papers are found and address issues
table(is.na(titleInd))

# identify the subset of paper with validation data
validationHumanReading <- validationHumanReading[!is.na(titleInd), ]
titleInd <- na.omit(unlist(titleInd))

validationHumanReading <- validationHumanReading[!duplicated(titleInd), ]
titleInd <- unique(titleInd)

validationTopicDocs <- topicDocs[titleInd, ]
validationDTM <- obj_dtm[titleInd, ]

## data prep

# remove QA'd out papers
if (scale_type != "location"){
	validationTopicDocs <- validationTopicDocs[validationHumanReading$country_location != 0, ]
	validationDTM <- validationDTM[validationHumanReading$country_location != 0, ]
	validationHumanReading <- validationHumanReading[validationHumanReading$country_location != 0, ]
}
# remove title, QA and location information
drops <- c("title", "country_location", "validation", "study_years")
validationHumanReading <- validationHumanReading[, !colnames(validationHumanReading) %in% drops]

# remove noisy information from human reading
if (scale_type == "temporal"){
	validationData <- do.call(data.frame, lapply(validationHumanReading, function(x) as.character(x))) 
	validationData <- do.call(data.frame, lapply(validationData, function(x) replace(x, which(!x %in% c("0", "1")), "0"))) 
	validationData <- do.call(data.frame, lapply(validationData, function(x) as.logical(as.integer(as.character(x))))) 
} else {
	validationData <- validationHumanReading
}


# changing names
colnames(validationTopicDocs) <- paste0("Topic", seq(ncol(validationTopicDocs)))

validationDTM <- as.matrix(validationDTM)
colnames(validationDTM) <- paste0("Term", seq(ncol(validationDTM)))

if (scale_type == "location"){
	l.df <- lapply(levels(validationData$Country.1), function(country){
		apply(validationData, MARGIN = 1, function(row) any(row == country))
	})
	trainingLabels <- do.call(cbind, l.df)
	colnames(trainingLabels) <- c("Irrelevant", levels(validationData$Country.1)[-1])
	trainingLabels <- data.frame(trainingLabels)
	trainingLabels <- trainingLabels[, which(sapply(apply(trainingLabels, 2, function(col) unique(col)), length) != 1)]
	trainingLabels <- trainingLabels[, which(apply(apply(trainingLabels, 2, function(col) table(col)), 2, min) != 1)]
	drops <- c("Bahamas", "Barbados", "Caribbean", "Haiti", "Trinidad.and.Tobago") # countries not in database
	trainingLabels <- trainingLabels[, which(!colnames(trainingLabels) %in% drops)]
} else {
	trainingLabels <- validationData
}
