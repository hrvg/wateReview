check_duplicate_row <- function(citation_dataframe){
	print(nrow(citation_dataframe))
	print(nrow(unique(citation_dataframe)))
	return(unique(citation_dataframe))
}

check_duplicate_title <- function(citation_dataframe){
	print(nrow(citation_dataframe))
	print(nrow(citation_dataframe[which(duplicated(citation_dataframe$Title) == FALSE) ,]))
	return(citation_dataframe[which(duplicated(citation_dataframe$Title) == FALSE) ,])
}

write_citation_dataframe <- function(root.dir, csv.dir){
	write.csv(citation_dataframe, file.path(root.dir, csv.dir, "citation_dataframe.csv"), row.names = FALSE)
}