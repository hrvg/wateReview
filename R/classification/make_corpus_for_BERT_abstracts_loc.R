# scale_type <- "spatial"
scale_type <- "location"
validationHumanReading <- read.csv(paste0("F:/hguillon/research/exploitation/R/latin_america/data/validation_df_", scale_type, ".csv"))
validationHumanReading <- validationHumanReading[validationHumanReading$title != "", ]

titleInd_file <- "F:/hguillon/research/exploitation/R/latin_america/data/titleInd.Rds"
titleInd <- readRDS(titleInd_file)

englishCorpus_file <- "F:/hguillon/research/exploitation/R/latin_america/data/english_corpus.Rds"
englishCorpus <- readRDS(englishCorpus_file)

in_corpus_file <- "in_corpus.Rds"
in_corpus <- readRDS(in_corpus_file)

EndNoteIdcorpus <- unname(sapply(in_corpus$pdfs, substr, start = 1, stop = 10))
EndNoteIdLDA <- unname(sapply(englishCorpus$fnames, substr, start = 1, stop = 10))

table(EndNoteIdLDA %in% EndNoteIdcorpus)

EndNoteIdLDA <- EndNoteIdLDA[which(EndNoteIdLDA %in% EndNoteIdcorpus)]
englishCorpus <- englishCorpus[which(EndNoteIdLDA %in% EndNoteIdcorpus), ]
englishCorpus$abstract <- as.character(in_corpus$abstract[match(EndNoteIdLDA, EndNoteIdcorpus)])

# check that all the papers are found and address issues
table(is.na(titleInd))

# identify the subset of paper with validation data
validationHumanReading <- validationHumanReading[!is.na(titleInd), ]
titleInd <- na.omit(unlist(titleInd))

validationHumanReading <- validationHumanReading[!duplicated(titleInd), ]
titleInd <- unique(titleInd)

validationCorpus <- englishCorpus[titleInd, ]

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
} else {
	trainingLabels <- validationData
}


keep <- c("fnames", "abstract")
validationCorpus <- validationCorpus[, colnames(validationCorpus) %in% keep]

# source("./R/classification/transform_temporal.R")

# trainingData <- cbind(validationCorpus, validationData)
trainingData <- cbind(validationCorpus, trainingLabels)


trainingData <- trainingData[which(!trainingData$abstract %in% c("NA", "NULL")), ]

print(head(trainingData[, -c(1:2)]))

testing_training_ratio <- 0

set.seed(1989)
ind_test <- seq(nrow(trainingData))
ind_test <- sample(ind_test, ceiling(testing_training_ratio * nrow(trainingData)))
ind_train <- setdiff(seq(nrow(trainingData)), ind_test)

testingData_file <- "F:/hguillon/research/exploitation/R/latin_america/data/test.csv"
write.csv(trainingData[ind_test, c(1:2)], testingData_file, row.names = FALSE)

testingLabels_file <- "F:/hguillon/research/exploitation/R/latin_america/data/test_labels.csv"
write.csv(trainingData[ind_test, -2], testingLabels_file, row.names = FALSE)

trainingData_file <- "F:/hguillon/research/exploitation/R/latin_america/data/train.csv"
write.csv(trainingData[ind_train, ], trainingData_file, row.names = FALSE)
