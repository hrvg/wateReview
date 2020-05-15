### libraries ###
library("mlr")
library("OpenML")
library("NLP")
library("tm")
library("data.table")
library("mldr")
library("dplyr")

### utils ###
import::here(.from = "./R/utils/lib_webscrapping.R",
  make.country_tokens,
  get.EndNoteIdcorpus,
  get.EndNoteIdLDA,
  QA.EndNoteIdCorpusLDA,
  align.dataWithEndNoteIdLDA,
  align.dataWithEndNoteIdcorpus,
  order.data
)

import::here(.from = "./R/utils/lib_MLR_predictions.R",
	consolidate_LDA_results,
	make_df_docs
)

# data loading
englishCorpus_file <- "F:/hguillon/research/exploitation/R/latin_america/data/english_corpus.Rds"
englishCorpus <- readRDS(englishCorpus_file)

in_corpus_file <- "in_corpus.Rds"
in_corpus <- readRDS(in_corpus_file)

predCountry <- readRDS("predCountry.Rds") # aligned with englishCorpus
predRelevance <- readRDS("predRelevance.Rds") # aligned with englishCorpus
topicDocs <- readRDS("./data/topicDocs.Rds") # aligned with englishCorpus

# get document IDs
EndNoteIdcorpus <- get.EndNoteIdcorpus(in_corpus)
EndNoteIdLDA <- get.EndNoteIdLDA(englishCorpus)
QA.EndNoteIdCorpusLDA(EndNoteIdLDA, EndNoteIdcorpus)

# align databases 
in_corpus <- align.dataWithEndNoteIdcorpus(in_corpus, EndNoteIdcorpus, EndNoteIdLDA)

englishCorpus <- align.dataWithEndNoteIdLDA(englishCorpus, EndNoteIdLDA, EndNoteIdcorpus)
predCountry <- align.dataWithEndNoteIdLDA(predCountry, EndNoteIdLDA, EndNoteIdcorpus)
predRelevance <- align.dataWithEndNoteIdLDA(predRelevance, EndNoteIdLDA, EndNoteIdcorpus)
topicDocs <- align.dataWithEndNoteIdLDA(topicDocs, EndNoteIdLDA, EndNoteIdcorpus)

EndNoteIdLDA <- align.dataWithEndNoteIdLDA(EndNoteIdLDA, EndNoteIdLDA, EndNoteIdcorpus)
EndNoteIdcorpus <- align.dataWithEndNoteIdcorpus(EndNoteIdcorpus, EndNoteIdcorpus, EndNoteIdLDA)

QA.EndNoteIdCorpusLDA(EndNoteIdLDA, EndNoteIdcorpus)

# order them according to LDA database
in_corpus <- order.data(in_corpus, EndNoteIdLDA, EndNoteIdcorpus)

saveRDS(predRelevance, "predRelevance.Rds")
saveRDS(predCountry %>% pull(response), "predCountry.Rds")
saveRDS(predCountry %>% select(-response), "predCountryMembership.Rds")

consolidate_LDA_results(theme_type = "theme", save = TRUE)
consolidate_LDA_results(theme_type = "NSF_general", save = TRUE)
consolidate_LDA_results(theme_type = "NSF_specific", save = TRUE)
consolidate_LDA_results(theme_type = "theme", description = "water budget", save = TRUE)
consolidate_LDA_results(theme_type = "theme", description = "methods", save = TRUE)
consolidate_LDA_results(theme_type = "theme", description = "spatial scale", save = TRUE)