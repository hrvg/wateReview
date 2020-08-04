#' Return the unique rows of a data.frame.
#' @param citation_dataframe a data.frame
#' @return a data.frame with non-duplicated rows
#' @export
check_duplicate_row <- function(citation_dataframe){
	print(nrow(citation_dataframe))
	print(nrow(unique(citation_dataframe)))
	return(unique(citation_dataframe))
}


#' Return a data.frame with unique Titles.
#' @param citation_dataframe a data.frame with a `Title` column
#' @return a data.frame with non-duplicated `Title`
#' @export
check_duplicate_title <- function(citation_dataframe){
	print(nrow(citation_dataframe))
	print(nrow(citation_dataframe[which(duplicated(citation_dataframe$Title) == FALSE) ,]))
	return(citation_dataframe[which(duplicated(citation_dataframe$Title) == FALSE) ,])
}

#' Save a data.frame.
#' @param csv.dir character or file.path to the location where to save the citation dataframe\
#' @export
write_citation_dataframe <- function(csv.dir){
	write.csv(citation_dataframe, file.path(csv.dir, "citation_dataframe.csv"), row.names = FALSE)
}