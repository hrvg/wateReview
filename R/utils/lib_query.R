get_csv_files <- function(root.dir, csv.dir){
	csv.files <- list.files(file.path(root.dir, csv.dir), pattern = ".csv", full.names = TRUE)
	return(csv.files)
}

get_citation_dataframe <- function(csv.files){
	dataframe_list <- lapply(csv.files, function(fname) read.csv(fname, header = TRUE))
	citation_dataframe <- do.call(rbind, dataframe_list)
	return(citation_dataframe)
}

get_language_dfs <- function(languages){
	import::here(.from = "./R/query_QA/helpers.R", read_citation_dataframe)
	dfs <- lapply(languages, read_citation_dataframe)
	names(dfs) <- languages
	return(dfs)
}

get_meta_df <- function(language_dfs){do.call(rbind, language_dfs)}

make_pretty_str <- function(string_list){
	# This function takes care of some formatting issue that appeared in the process of aligning database coming from the query and from EndNote.
	# The function removes alpha-numeric characters, some special characters, trim whitespaces and concatenate them.
	# arg in: a list of string
	# output: a cleaned list of string 
	nl <- gsub("[^[:alnum:][:space:]]", "",  string_list)
	nl <- gsub("ltigt", " ",  nl)
	nl <- gsub("\\s+", " ", nl)
	nl <- trimws(nl)
	return(nl)
}