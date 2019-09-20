theme_df_docs <- readRDS("data/theme_df_docs.Rds")

in_corpus_file <- "in_corpus.Rds"
in_corpus <- readRDS(in_corpus_file)

EndNoteIdcorpus <- unname(sapply(in_corpus$pdfs, substr, start = 1, stop = 10))
EndNoteIdLDA <- unname(sapply(titleDocs, substr, start = 1, stop = 10))

in_corpus_LDA <- in_corpus[which(EndNoteIdcorpus %in% EndNoteIdLDA), ]

missing <- which(! EndNoteIdLDA %in% EndNoteIdcorpus)

theme_df_docs <- theme_df_docs[-missing, ]
year <- in_corpus_LDA$Year
predCountry <- predCountry[-missing]
predCountryMembership <- totPred$data[-missing, colnames(predCountryMembership) != "response"]
saveRDS(predCountryMembership, "predCountryMembership.Rds")

consolidated_results <- data.frame(year = year, country = as.character(predCountry))
consolidated_results <- cbind(theme_df_docs, consolidated_results)
saveRDS(consolidated_results, "consolidated_results.Rds")