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
  align.dataWithEndNoteIdLDA,
  align.dataWithEndNoteIdcorpus,
  order.data,
  make.webscrapped_trainingData,
  get.ind_hasCountryTag
)

import::here(.from = "./R/utils/lib_webscrapping.R",
  get.scopusAbstract,
  get.wosAbstract,
  get.wosAuthKeywords,
  get.wosFullResult,
  get.allMetadata,
  add.abstractsToCorpus,
  get.relevantCountries,
  get.allAuthKeywords,
  QA.AuthKeywords,
  get.boolean_AuthKeywords,
)

### main ###

# data loading
englishCorpus_file <- "F:/hguillon/research/exploitation/R/latin_america/data/english_corpus.Rds"
englishCorpus <- readRDS(englishCorpus_file)

in_corpus_file <- "in_corpus.Rds"
in_corpus <- readRDS(in_corpus_file)

englishCorpus <- benglishCorpus
in_corpus <- bin_corpus

boolean_AuthKeywords <- readRDS("boolean_AuthKeywords.Rds")

# get document IDs
EndNoteIdcorpus <- get.EndNoteIdcorpus(in_corpus)
EndNoteIdLDA <- get.EndNoteIdLDA(englishCorpus)
QA.EndNoteIdCorpusLDA(EndNoteIdLDA, EndNoteIdcorpus)

# align databases 
in_corpus <- align.dataWithEndNoteIdcorpus(in_corpus, EndNoteIdcorpus, EndNoteIdLDA)
boolean_AuthKeywords <- align.dataWithEndNoteIdcorpus(boolean_AuthKeywords, EndNoteIdcorpus, EndNoteIdLDA)

englishCorpus <- align.dataWithEndNoteIdLDA(englishCorpus, EndNoteIdLDA, EndNoteIdcorpus)

EndNoteIdLDA <- align.dataWithEndNoteIdLDA(EndNoteIdLDA, EndNoteIdLDA, EndNoteIdcorpus)
EndNoteIdcorpus <- align.dataWithEndNoteIdcorpus(EndNoteIdcorpus, EndNoteIdcorpus, EndNoteIdLDA)

QA.EndNoteIdCorpusLDA(EndNoteIdLDA, EndNoteIdcorpus)

# order them according to LDA database
boolean_AuthKeywords <- order.data(boolean_AuthKeywords, EndNoteIdLDA, EndNoteIdcorpus)
in_corpus <- order.data(in_corpus, EndNoteIdLDA, EndNoteIdcorpus)
englishCorpus$abstract <- in_corpus$abstract
ind_hasCountryTag <- get.ind_hasCountryTag(boolean_AuthKeywords)

# make the webscrapped training data
webscrapped_trainingData <-  make.webscrapped_trainingData(boolean_AuthKeywords, ind_hasCountryTag, englishCorpus, englishCorpus_file)

country_tokens <- webscrapped_trainingData$country_tokens
webscrapped_validationDTM <- webscrapped_trainingData$webscrapped_validationDTM
webscrapped_trainingLabels <- webscrapped_trainingData$webscrapped_trainingLabels

saveRDS(country_tokens, "country_tokens.Rds")
saveRDS(webscrapped_validationDTM, "webscrapped_validationDTM.Rds")
saveRDS(webscrapped_trainingLabels, "webscrapped_trainingLabels.Rds")