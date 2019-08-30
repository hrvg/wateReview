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

ind_hasCountryTag <- readRDS("ind_hasCountryTag.Rds")
boolean_AuthKeywords <- readRDS("boolean_AuthKeywords.Rds")

ind_hasCountryTag <- ind_hasCountryTag[match(EndNoteIdLDA, EndNoteIdcorpus)]
boolean_AuthKeywords <- boolean_AuthKeywords[match(EndNoteIdLDA, EndNoteIdcorpus), ]

englishCorpus_hasTag <- englishCorpus[ind_hasCountryTag, ]
boolean_AuthKeywords_hasTag <- boolean_AuthKeywords[ind_hasCountryTag, ]

irrelevant <- rep(FALSE, nrow(boolean_AuthKeywords_hasTag))
webscrapped_trainingLabels <- cbind(irrelevant, boolean_AuthKeywords_hasTag)
colnames(webscrapped_trainingLabels) <- c("Irrelevant", colnames(boolean_AuthKeywords_hasTag))

# removing NULL abstract
ind_nonNullnonNA <- which(!(englishCorpus_hasTag$abstract == "NULL" | englishCorpus_hasTag$abstract == "NA"))
englishCorpus_hasTag <- englishCorpus_hasTag[ind_nonNullnonNA, ]
webscrapped_trainingLabels <- webscrapped_trainingLabels[ind_nonNullnonNA, ]

# libraries
library(mlr)
library(OpenML)
library(NLP)
library(tm)
library(data.table)
library(mldr)

titleInd_file <- "F:/hguillon/research/exploitation/R/latin_america/data/titleInd.Rds"
titleInd <- readRDS(titleInd_file)
englishCorpus_complete <- readRDS(englishCorpus_file)

ind_HumanRead_hasTag <- which(englishCorpus_hasTag$fnames %in% englishCorpus[titleInd, ]$fnames)

# removing human read files

englishCorpus_hasTag <- englishCorpus_hasTag[-ind_HumanRead_hasTag, ]
webscrapped_trainingLabels <- webscrapped_trainingLabels[-ind_HumanRead_hasTag, ]

dtm_file <- "F:/hguillon/research/exploitation/R/latin_america/data/obj_dtm_from_dfm_geo.Rds"
obj_dtm <- readRDS(dtm_file)

webscrapped_validationDTM <- obj_dtm[which(englishCorpus_complete$fnames %in% englishCorpus_hasTag$fnames), ]
webscrapped_validationDTM <- as.matrix(webscrapped_validationDTM)
colnames(webscrapped_validationDTM) <- paste0("Term", seq(ncol(webscrapped_validationDTM)))
colnames(webscrapped_trainingLabels) <- gsub(" ", ".", colnames(webscrapped_trainingLabels))