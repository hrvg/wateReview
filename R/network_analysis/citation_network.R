# libs
library("bibliometrix")

# loading citation network
biblioDir <- "/media/hguillon/hrvg/research/data/latin_america/corpus_csv/english"
biblioFile <- "english_citation.bib"
# biblioFile <- "scopus_english_english.csv"
biblioNetwork <- readFiles(file.path(biblioDir, biblioFile))
biblio_df <- convert2df(file = biblioNetwork, dbsource = "scopus", format = "plaintext")
biblio_df <- bib2df(biblioNetwork)
results <- biblioAnalysis(biblio_df)
summary(object = results, k = 10, pause = FALSE)

# load paper id
in_corpus_file <- "in_corpus.Rds"
in_corpus <- readRDS(in_corpus_file)
EndNoteIdcorpus <- unname(sapply(in_corpus$pdfs, substr, start = 1, stop = 10))
EndNoteIdLDA <- unname(sapply(titleDocs, substr, start = 1, stop = 10))
in_corpus_LDA <- in_corpus[which(EndNoteIdcorpus %in% EndNoteIdLDA), ]
missing <- which(! EndNoteIdLDA %in% EndNoteIdcorpus)

# loading consolidated results
consolidated_results <- readRDS("consolidated_results.Rds")
consolidated_results$ID <- EndNoteIdLDA[-missing]
consolidated_results <- consolidated_results[which(consolidated_results$country != "Irrelevant"), ]




# 
docTopics <- consolidated_results[, seq(62)]
country <- consolidated_results$country
country <- as.character(country[country != "Irrelevant"])