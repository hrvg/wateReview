get_csv_files <- function(root.dir, csv.dir){
	csv.files <- list.files(file.path(root.dir, csv.dir), pattern = ".csv", full.names = TRUE)
	return(csv.files)
}

get_citation_dataframe <- function(csv.files){
	dataframe_list <- lapply(csv.files, function(fname) read.csv(fname, header = TRUE))
	citation_dataframe <- do.call(rbind, dataframe_list)
	return(citation_dataframe)
}