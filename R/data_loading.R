#' List .csv files in a directory
#' @param csv.dir character or file.path, target directory
#' @return character, file names
#' @export
get_csv_files <- function(csv.dir){
	csv.files <- list.files(file.path(root.dir, csv.dir), pattern = ".csv", full.names = TRUE)
	return(csv.files)
}

#' Read .csv files to create a citation data.frame
#' @param csv.files character, file names
#' @return A data.frame
#' @export
get_citation_dataframe <- function(csv.files){
	dataframe_list <- lapply(csv.files, function(fname) read.csv(fname, header = TRUE))
	citation_dataframe <- do.call(rbind, dataframe_list)
	return(citation_dataframe)
}

#' Read the citation data frame and store them into a named list
#' @param languages character, of one "english", "spanish", "portuguese"
#' @return a named list with `length(languages)` elements, each a data.frame
#' @export
get_language_dfs <- function(languages){
	dfs <- lapply(languages, read_citation_dataframe)
	names(dfs) <- languages
	return(dfs)
}

#' Binds the separate languages data.frame into a meta data.frame
#' @param language_dfs a named list of data.frame
#' @return a data.frame
#' @export
get_meta_df <- function(language_dfs){
	language_dfs <- lapply(language_dfs, function(df){
		df %>% 
		dplyr::mutate_if(is.factor, iconv, from = "WINDOWS-1252", to = "UTF-8") %>%
		dplyr::mutate_if(is.character, iconv, from = "WINDOWS-1252", to = "UTF-8")
	})
	do.call(rbind, language_dfs)
}

#' This function performs the cross-walk between human reading databases and topic model databases using document titles
#' @param titleInd_file a filename, if it exists, file content is read, if it does not exist the formatting and regexpr matching is performed
#' @param humanReadingDatabase human reading database with column "title", default to NULL
#' @param topicModelTitles titles of the topic model files, default to NULL
#' @return matching indices between the two databases
#' @export
get_titleInd <- function(
	titleInd_file = "F:/hguillon/research/exploitation/R/latin_america/data/titleInd.Rds",
	humanReadingDatabase = NULL,
	topicModelTitles = NULL
	){
	if(!file.exists(titleInd_file)){
		# modify the format of titles so that match between topicModelTitles and humanReadingDatabase
		titleHumanReading <- as.character(humanReadingDatabase$title)
		titleHumanReading <- gsub(".pdf", "", titleHumanReading)
		titleHumanReading <- gsub(" ", "_", titleHumanReading)
		titleHumanReading <- gsub("[^\x20-\x7E]", "", titleHumanReading)
		titleHumanReading <- gsub("\\(", "", titleHumanReading)
		titleHumanReading <- gsub("\\)", "", titleHumanReading)
		titleHumanReading <- gsub("\\'", "", titleHumanReading)
		titleHumanReading <- gsub(",", "", titleHumanReading)
		# look for matches
		titleInd <- sapply(titleHumanReading, function(t) grep(t, topicModelTitles)[1])
		saveRDS(titleInd, titleInd_file)
	} else {
		titleInd <- readRDS(titleInd_file)
	}
	return(titleInd)
}

#' Read human reading database
#' @param scale_type One of "location", "spatial", "temporal", default to "location"
#' @return validation human-read data
#' @export
get_validationHumanReading <- function(scale_type = "location"){
	validationHumanReading <- read.csv(paste0("F:/hguillon/research/exploitation/R/latin_america/data/validation_df_", scale_type, ".csv"))
	validationHumanReading <- validationHumanReading[validationHumanReading$title != "", ]
	return(validationHumanReading)
}

#' Read topic model data
#' @param fname path to the topic model data
#' @return topic model data
#' @export
get_topicDocs <- function(fname = "F:/hguillon/research/exploitation/R/latin_america/data/topicDocs.Rds"){
	topicDocs <- readRDS(fname)
	return(topicDocs)
}

#' Read topic model file titles
#' @param fname path to the topic model file titles
#' @return topic model file titles
#' @export
get_titleDocs <- function(topicDocs, fname = "F:/hguillon/research/exploitation/R/latin_america/data/info.dat"){
	titleDocs <- readLines(fname)
	if (nrow(topicDocs) != length(titleDocs)) warning("Dimensions not matching")
	return(titleDocs)
}

#' read training data (document-term matrix corresponding to webscrapped labels)
#' @param fname path to training data (document-term matrix corresponding to webscrapped labels)
#' @return training data (document-term matrix corresponding to webscrapped labels)
#' @export
get_webscrapped_validationDTM <- function(fname = "webscrapped_validationDTM.Rds"){
	webscrapped_validationDTM <- readRDS("webscrapped_validationDTM.Rds")
	return(webscrapped_validationDTM)
}

#' read webscrapped training labels
#' @param fname path to webscrapped training labels data
#' @return webscrapped training labels
#' @export
get_webscrapped_trainingLabels <- function(fname = "webscrapped_trainingLabels.Rds"){
	webscrapped_trainingLabels <- readRDS("webscrapped_trainingLabels.Rds")
	return(webscrapped_trainingLabels)
}

#' read document-term matrix created by the text mining code
#' @param dtm_file path to saved document-term matrix
#' @return document-term matrix
#' @export
get_DocTermMatrix <- function(dtm_file= "F:/hguillon/research/exploitation/R/latin_america/data/obj_dtm_from_dfm_country.Rds"){
	obj_dtm <- readRDS(dtm_file)
	return(obj_dtm)
}

#' identifies the subset of paper with validation data and align databases
#' @param titleInd cross-walked indices between human-reading database and topic model
#' @param validationHumanReading human-reading database
#' @param topicDocs results from the topic model
#' @param DTM document-term matrix derived from topic modelled corpus
#' @return list with four elements: titleInd, validationHumanReading, validationTopicDocs, validationDTM
#' @export
align.humanReadingTopicModel <- function(titleInd, validationHumanReading, topicDocs, DTM){
	validationHumanReading <- validationHumanReading[!is.na(titleInd), ]
	titleInd <- na.omit(unlist(titleInd))

	validationHumanReading <- validationHumanReading[!duplicated(titleInd), ]
	titleInd <- unique(titleInd)

	validationTopicDocs <- topicDocs[titleInd, ]
	validationDTM <- DTM[titleInd, ]

	res <- list(titleInd = titleInd,
		validationHumanReading = validationHumanReading,
		validationTopicDocs = validationTopicDocs,
		validationDTM = validationDTM)
	return(res)
}

#' Perform QA/QC on aligned data
#' @param alignedData list of aligned data between human reading and topic model
#' @param scale_type One of "location", "spatial", "temporal", default to "location"
#' @return list with three elements: validationHumanReading, validationTopicDocs, validationDTM
#' @export
QA.alignedData <- function(alignedData, scale_type = "location"){
	validationHumanReading <- alignedData$validationHumanReading
	validationTopicDocs <- alignedData$validationTopicDocs
	validationDTM <- alignedData$validationDTM
	# remove QA'd out papers
	if (scale_type != "location"){
		validationTopicDocs <- validationTopicDocs[validationHumanReading$country_location != 0, ]
		validationDTM <- validationDTM[validationHumanReading$country_location != 0, ]
		validationHumanReading <- validationHumanReading[validationHumanReading$country_location != 0, ]
	}
	# remove title, QA and location information
	drops <- c("title", "country_location", "validation", "study_years")
	validationHumanReading <- validationHumanReading[, !colnames(validationHumanReading) %in% drops]

	# remove noisy information from human reading
	if (scale_type %in% c("temporal", "spatial")){
		validationHumanReading <- do.call(data.frame, lapply(validationHumanReading, function(x) as.character(x))) 
		validationHumanReading <- do.call(data.frame, lapply(validationHumanReading, function(x) replace(x, which(!x %in% c("0", "1")), "0"))) 
		validationHumanReading <- do.call(data.frame, lapply(validationHumanReading, function(x) as.logical(as.integer(as.character(x))))) 
	}

	# changing colnames
	colnames(validationTopicDocs) <- paste0("Topic", seq(ncol(validationTopicDocs)))

	validationDTM <- as.matrix(validationDTM)
	# colnames(validationDTM) <- paste0("Term", seq(ncol(validationDTM)))
	res <- list(validationHumanReading = validationHumanReading,
		validationTopicDocs = validationTopicDocs,
		validationDTM = validationDTM) 
	return(res)
}

