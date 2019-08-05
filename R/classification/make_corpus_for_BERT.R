# scale_type <- "spatial"
scale_type <- "temporal"
validationHumanReading <- read.csv(paste0("F:/hguillon/research/exploitation/R/latin_america/data/validation_df_", scale_type, ".csv"))
validationHumanReading <- validationHumanReading[validationHumanReading$title != "", ]

titleInd_file <- "F:/hguillon/research/exploitation/R/latin_america/data/titleInd.Rds"
titleInd <- readRDS(titleInd_file)

englishCorpus_file <- "F:/hguillon/research/exploitation/R/latin_america/data/english_corpus.Rds"
englishCorpus <- readRDS(englishCorpus_file)

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
validationCorpus <- validationCorpus[validationHumanReading$country_location != 0, ]
validationHumanReading <- validationHumanReading[validationHumanReading$country_location != 0, ]

# remove title, QA and location information
drops <- c("title", "country_location", "validation", "study_years")
validationHumanReading <- validationHumanReading[, !colnames(validationHumanReading) %in% drops]

# remove noisy information from human reading
validationData <- do.call(data.frame, lapply(validationHumanReading, function(x) as.character(x))) 
validationData <- do.call(data.frame, lapply(validationData, function(x) replace(x, which(!x %in% c("0", "1")), "0"))) 
validationData <- do.call(data.frame, lapply(validationData, function(x) (as.integer(as.character(x))))) 

keep <- c("fnames", "clean")
validationCorpus <- validationCorpus[, colnames(validationCorpus) %in% keep]

trainingData <- cbind(validationCorpus, validationData)

testing_training_ratio <- 0.15

trainingData_file <- "F:/hguillon/research/exploitation/R/latin_america/data/training.csv"
write.csv(trainingData[c(ceiling(testing_training_ratio * nrow(trainingData)):nrow(trainingData)), ], trainingData_file, row.names = FALSE)

testingData_file <- "F:/hguillon/research/exploitation/R/latin_america/data/testing.csv"
write.csv(trainingData[c(1:floor(testing_training_ratio * nrow(trainingData))), c(1:2)], testingData_file, row.names = FALSE)

testingLabels_file <- "F:/hguillon/research/exploitation/R/latin_america/data/test_labels.csv"
write.csv(trainingData[c(1:floor(testing_training_ratio * nrow(trainingData))), c(1:2)], testingData_file, row.names = FALSE)