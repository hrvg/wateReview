check_duplicate <- function(citation_dataframe){
	return(unique(citation_dataframe))
}

write_citation_dataframe <- function(root.dir, csv.dir){
	write.csv(citation_dataframe, file.path(root.dir, csv.dir, "citation_dataframe.csv"), row.names = FALSE)
}