#' Retrieve or create a document-feature matrix (dfm) from hard coded options relevant to current project
#' @param dfm_file Path to a dfm file, if file exists, file is read, if not dfm is computed from corpus
#' @param corpus_file path to a corpus data.frame with text in text column
#' @param text_col text column in the corpus data.frame
#' @return document-feature matrix extracted 
#' @export
get_dfm <- function(
	dfm_file = "F:/hguillon/research/exploitation/R/latin_america/data/obj_dfm.Rds", 
	corpus_file = "F:/hguillon/research/exploitation/R/latin_america/data/english_corpus.Rds",
	text_col = "raw"){
	if (!file.exists(dfm_file)){
		corpus_df <- readRDS(corpus_file)
		obj_dfm <- quanteda::dfm(corpus_df[[text_col]], 
			tolower = TRUE,
			stem = TRUE,
			remove = c(quanteda::stopwords("en"), quanteda::stopwords("es"), quanteda::stopwords("pt")),
			remove_punct = TRUE,
			remove_symbols = TRUE,
			remove_numbers = TRUE,
			remove_separators = TRUE,
			remove_hyphens = TRUE,
			remove_url = TRUE)

		obj_dfm <- quanteda::dfm_remove(obj_dfm, min_nchar = 4)
		saveRDS(obj_dfm, dfm_file)
	} else {
		obj_dfm <- readRDS(dfm_file)
	}
	return(obj_dfm)
}

#' Filter the complete document-feature matrix to retain all features with occurence higher than the lowest occurence of country tokens.
#' This function mainly serves to limit the size of the document-feature matrix
#' @param obj_dfm a document-feature matrix
#' @param country_tokens the tokens with which the document-feature matrix is filtered
#' @return a filtered document-feature matrix
#' @export
filter_dfm <- function(obj_dfm, country_tokens = NULL){
	if (is.null(country_tokens)) country_tokens <- readRDS("./data/country_tokens.Rds")
	sorted_dfm <- quanteda::dfm_sort(obj_dfm, decreasing = TRUE, margin = c("features"))
	country_max <- max(sapply(tolower(country_tokens), function(c) which(quanteda::featnames(sorted_dfm) == c)))
	filtered_dfm <- sorted_dfm[, 1:country_max]
	return(filtered_dfm)
}

#' Get document-term matrix from a document-feature matrix and a list of tokens
#' @param obj_dfm a document-feature matrix
#' @param filtering_tokens a list of tokens which are match against the features of the document-feature matrix
#' @return a document-term matrix with nrow == nrow(obj_dfm) and ncol == length(filtering_tokens)
#' @export
get_dtm <- function(obj_dfm, filtering_tokens){
	obj_dtm <- obj_dfm[, na.omit(match(filtering_tokens, quanteda::featnames(obj_dfm)))]
	return(obj_dtm)
}
