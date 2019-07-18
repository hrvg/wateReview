# libraries
library(mlr)
library(OpenML)
library(NLP)
library(tm)
library(data.table)

# main



# get DTM
# lines <- readLines("F:/hguillon/research/exploitation/R/latin_america/data/corpus.dat")
# vec <- VectorSource(lines)
# obj_corpus <- Corpus(vec)
# obj_dtm <- DocumentTermMatrix(obj_corpus)
# saveRDS <- saveRDS(obj_dtm"F:/hguillon/research/exploitation/R/latin_america/data/obj_dtm.Rds")

# # modify the format of titles so that match between titleDocs and validationHumanReading
# titleValidation <- as.character(validationHumanReading$title)
# titleValidation <- gsub(".pdf", "", titleValidation)
# titleValidation <- gsub(" ", "_", titleValidation)
# titleValidation <- gsub("[^\x20-\x7E]", "", titleValidation)
# titleValidation <- gsub("\\(", "", titleValidation)
# titleValidation <- gsub("\\)", "", titleValidation)
# titleValidation <- gsub("\\'", "", titleValidation)
# titleValidation <- gsub(",", "", titleValidation)

# # look for matches
# titleInd <- sapply(titleValidation, function(t) grep(t, titleDocs)[1])

## data prep

# read data
topicDocs <- readRDS("F:/hguillon/research/exploitation/R/latin_america/data/topicDocs.Rds")
titleDocs <- readLines("F:/hguillon/research/exploitation/R/latin_america/data/info.dat")
if (nrow(topicDocs) != length(titleDocs)) warning("Dimensions not matching")
validationHumanReading <- read.csv("F:/hguillon/research/exploitation/R/latin_america/data/validation_df_temporal.csv")
validationHumanReading <- validationHumanReading[validationHumanReading$title != "", ]

obj_dtm <- readRDS("F:/hguillon/research/exploitation/R/latin_america/data/obj_dtm.Rds")

titleInd <- readRDS("F:/hguillon/research/exploitation/R/latin_america/data/titleInd.Rds")

# check that all the papers are found and address issues
table(is.na(titleInd))

# identify the subset of paper with validation data

validationHumanReading <- validationHumanReading[!is.na(titleInd), ]
titleInd <- na.omit(unlist(titleInd))

validationHumanReading <- validationHumanReading[!duplicated(titleInd), ]
titleInd <- unique(titleInd)

validationTopicDocs <- topicDocs[titleInd, ]
validationDTM <- obj_dtm[titleInd, ]

# remove QA'd out papers
validationTopicDocs <- validationTopicDocs[validationHumanReading$country_location != 0, ]
validationDTM <- validationDTM[validationHumanReading$country_location != 0, ]
validationHumanReading <- validationHumanReading[validationHumanReading$country_location != 0, ]

# remove title, QA and location information
drops <- c("title", "country_location", "validation", "study_years")
validationHumanReading <- validationHumanReading[, !colnames(validationHumanReading) %in% drops]

# remove noisy information from human reading
validationData <- do.call(data.frame, lapply(validationHumanReading, function(x) as.character(x))) 
validationData <- do.call(data.frame, lapply(validationData, function(x) replace(x, which(!x %in% c("0", "1")), "0"))) 
validationData <- do.call(data.frame, lapply(validationData, function(x) as.logical(as.integer(as.character(x))))) 
# changing names
colnames(validationTopicDocs) <- paste0("Topic", seq(ncol(validationTopicDocs)))

validationDTM <- as.matrix(validationDTM)
colnames(validationDTM) <- paste0("Term", seq(ncol(validationDTM)))

# preProc <- preProcess(validationDTM, method = c("nzv", "zv"))
# print(preProc)
# library(caret)
# validationDTM <- predict(preProc, validationDTM)
# validationDTM <- as.data.frame(validationDTM)

# bind the validation data to the topicDocs
validationHumanReadingTopicDocs <- cbind(validationTopicDocs, validationData)
validationHumanReadingDTM <- cbind(validationDTM, validationData)

# remove title, QA and location information
target <- colnames(validationData)
## SVM model or ANN

set.seed(1789)
spatial.task = makeMultilabelTask(data = validationHumanReadingDTM, target = target)
spatial.task = makeMultilabelTask(data = validationHumanReadingTopicDocs, target = target)
binary.learner = makeLearner("classif.svm")
lrncc = makeMultilabelClassifierChainsWrapper(binary.learner)
# rdesc = makeResampleDesc("RepCV", folds = 10, reps = 10)
rdesc = makeResampleDesc("CV", iters = 10)
r = resample(lrncc, spatial.task, rdesc, measures = multilabel.acc)



n = getTaskSize(spatial.task)
train.set = seq(1, n, by = 2)
test.set = seq(2, n, by = 2)

spatial.mod.cc = train(lrncc, spatial.task, subset = train.set)
spatial.pred.cc = predict(spatial.mod.cc, task = spatial.task, subset = test.set)


performance(spatial.pred.cc, measures = list(multilabel.hamloss, multilabel.subset01, multilabel.f1, multilabel.acc))

lrnbr = makeMultilabelBinaryRelevanceWrapper(binary.learner)

spatial.mod.br = train(lrnbr, spatial.task, subset = train.set)
spatial.pred.br = predict(spatial.mod.br, task = spatial.task, subset = test.set)

performance(spatial.pred.br, measures = list(multilabel.hamloss, multilabel.subset01, multilabel.f1, multilabel.acc))



r = resample(lrnbr, spatial.task, rdesc, measures = multilabel.acc)


