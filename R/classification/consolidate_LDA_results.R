consolidate_LDA_results <- function(theme_type = "theme", description = NULL, save = FALSE){
	theme_df_docs <- make_df_docs(theme_type = theme_type, description = description)
	in_corpus_file <- "in_corpus.Rds"
	in_corpus <- readRDS(in_corpus_file)
	EndNoteIdcorpus <- unname(sapply(in_corpus$pdfs, substr, start = 1, stop = 10))
	titleDocs <- readLines("data/info.dat")
	EndNoteIdLDA <- unname(sapply(titleDocs, substr, start = 1, stop = 10))
	in_corpus_LDA <- in_corpus[which(EndNoteIdcorpus %in% EndNoteIdLDA), ]
	missing <- which(! EndNoteIdLDA %in% EndNoteIdcorpus)
	theme_df_docs <- theme_df_docs[-missing, ]
	year <- in_corpus_LDA$Year
	consolidated_results <- data.frame(year = year, country = as.character(predCountry))
	consolidated_results <- cbind(theme_df_docs, consolidated_results)
	if (save){
		saveRDS(consolidated_results, paste0("consolidated_results_", theme_type, ".Rds"))
	} else {
		return(consolidated_results)
	}
}

get_predCountry <- function(){
	predCountryMembership <- readRDS("predCountryMembership.Rds")
	predCountry <- colnames(predCountryMembership)[apply(predCountryMembership, 1, which.max)]
	predCountry <- gsub("prob.", "", predCountry)
}

save_predCountryMembership <- function(predCountryMembership){
	in_corpus_file <- "in_corpus.Rds"
	in_corpus <- readRDS(in_corpus_file)
	EndNoteIdcorpus <- unname(sapply(in_corpus$pdfs, substr, start = 1, stop = 10))
	EndNoteIdLDA <- unname(sapply(titleDocs, substr, start = 1, stop = 10))
	in_corpus_LDA <- in_corpus[which(EndNoteIdcorpus %in% EndNoteIdLDA), ]
	missing <- which(! EndNoteIdLDA %in% EndNoteIdcorpus)
	year <- in_corpus_LDA$Year
	predCountry <- predCountry[-missing]
	predCountryMembership <- totPred$data[-missing, colnames(predCountryMembership) != "response"]
	saveRDS(predCountryMembership, "predCountryMembership.Rds")
}

make_df_docs <- function(theme_type = "theme", description = NULL, save = FALSE){
	stopifnot(theme_type %in% c("topic_name", "theme", "NSF_specific", "NSF_general"))
	if (!is.null(description)){
		stopifnot(description %in% c("spatial scale", "methods", "temporal scale", "water budget"))
	}
	topicDocs <- readRDS("./data/topicDocs.Rds")
	topic_names <- read.csv("./data/topic_names.csv")
	topic_names$description <- as.character(topic_names$description)
	topic_names <- topic_names[match(seq(nrow(topic_names)), topic_names$topic_id), ]
	topicDocs <- as.data.frame(topicDocs)
	colnames(topicDocs) <- topic_names$topic_name
	if (!is.null(description)){
		theme_type <- "topic_name"
		themes <- unique(topic_names[[theme_type]][grepl(description, topic_names$description)])
	} else {
		themes <- unique(topic_names[[theme_type]][!is.na(topic_names[[theme_type]])])
	}
	theme_df_docs <- lapply(themes, function(th){
	    ind <- which(topic_names[[theme_type]] == th)
	    if (length(ind) == 1){
	      return(topicDocs[, ind])
	    } else {
	      return(rowSums(topicDocs[, ind]))
	    }
	})
	theme_df_docs <- do.call(cbind, theme_df_docs)
	rownames(theme_df_docs) <- rownames(topicDocs)
	colnames(theme_df_docs) <- themes
	rm(topicDocs)
	rSums <- rowSums(theme_df_docs)
	theme_df_docs <- sweep(theme_df_docs, 1, rSums, "/")
	if (save){
		saveRDS(object = theme_df_docs, paste0("./data/", ifelse(is.null(description), theme_type, description), "_df_docs.Rds"))
	} else {
		return(theme_df_docs)
	}
}

save_predCountryMembership(predCountryMembership)
predCountry <- get_predCountry()