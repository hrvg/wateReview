### libraries ###
library("mlr")
library("OpenML")
library("NLP")
library("tm")
library("data.table")
library("mldr")

### utils ###
import::here(.from = "./R/utils/lib_webscrapping.R",
  make.country_tokens,
  get.EndNoteIdcorpus,
  get.EndNoteIdLDA,
  QA.EndNoteIdCorpusLDA,
  align.EndNoteIdLDA,
  align.EndNoteIdcorpus,
  align.englishCorpus,
  align.data,
  make.webscrapped_trainingData
)

### main ###

# data loading
englishCorpus_file <- "F:/hguillon/research/exploitation/R/latin_america/data/english_corpus.Rds"
englishCorpus <- readRDS(englishCorpus_file)

in_corpus_file <- "in_corpus.Rds"
in_corpus <- readRDS(in_corpus_file)

ind_hasCountryTag <- readRDS("ind_hasCountryTag.Rds")
boolean_AuthKeywords <- readRDS("boolean_AuthKeywords.Rds")

# get document IDs
EndNoteIdcorpus <- get.EndNoteIdcorpus(in_corpus)
EndNoteIdLDA <- get.EndNoteIdLDA(englishCorpus)
QA.EndNoteIdCorpusLDA(EndNoteIdLDA, EndNoteIdcorpus)

# align databases
EndNoteIdLDA <- align.EndNoteIdLDA(EndNoteIdLDA, EndNoteIdcorpus)
EndNoteIdcorpus <- align.EndNoteIdcorpus(EndNoteIdcorpus, EndNoteIdLDA)
englishCorpus <- align.englishCorpus(englishCorpus, EndNoteIdLDA, EndNoteIdcorpus, in_corpus)
ind_hasCountryTag <- align.data(ind_hasCountryTag, EndNoteIdLDA, EndNoteIdcorpus)
boolean_AuthKeywords <- align.data(boolean_AuthKeywords, EndNoteIdLDA, EndNoteIdcorpus)

# make the webscrapped training data
webscrapped_trainingData <-  make.webscrapped_trainingData(boolean_AuthKeywords, ind_hasCountryTag, englishCorpus, englishCorpus_file)

country_tokens <- webscrapped_trainingData$country_tokens
webscrapped_validationDTM <- webscrapped_trainingData$webscrapped_validationDTM
webscrapped_trainingLabels <- webscrapped_trainingData$webscrapped_trainingLabels

saveRDS(country_tokens, "country_tokens.Rds")
saveRDS(webscrapped_validationDTM, "webscrapped_validationDTM.Rds")
saveRDS(webscrapped_trainingLabels, "webscrapped_trainingLabels.Rds")