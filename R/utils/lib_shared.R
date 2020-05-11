#' This function performs the cross-walk between human reading databases and topic model databases using document titles
#' @param titleInd_file a filename, if it exists, file content is read, if it does not exist the formatting and regexpr matching is performed
#' @param humanReadingDatabase human reading database with column "title", default to NULL
#' @param topicModelTitles titles of the topic model files, default to NULL
#' @return matching indices between the two databases
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
get_validationHumanReading <- function(scale_type = "location"){
	validationHumanReading <- read.csv(paste0("F:/hguillon/research/exploitation/R/latin_america/data/validation_df_", scale_type, ".csv"))
	validationHumanReading <- validationHumanReading[validationHumanReading$title != "", ]
	return(validationHumanReading)
}

#' Read topic model data
#' @param fname path to the topic model data
#' @return topic model data
get_topicDocs <- function(fname = "F:/hguillon/research/exploitation/R/latin_america/data/topicDocs.Rds"){
	topicDocs <- readRDS(fname)
	return(topicDocs)
}

#' Read topic model file titles
#' @param fname path to the topic model file titles
#' @return topic model file titles
get_titleDocs <- function(topicDocs, fname = "F:/hguillon/research/exploitation/R/latin_america/data/info.dat"){
	titleDocs <- readLines(fname)
	if (nrow(topicDocs) != length(titleDocs)) warning("Dimensions not matching")
	return(titleDocs)
}

#' read training data (document-term matrix corresponding to webscrapped labels)
#' @param fname path to training data (document-term matrix corresponding to webscrapped labels)
#' @return training data (document-term matrix corresponding to webscrapped labels)
get_webscrapped_validationDTM <- function(fname = "webscrapped_validationDTM.Rds"){
	webscrapped_validationDTM <- readRDS("webscrapped_validationDTM.Rds")
	return(webscrapped_validationDTM)
}

#' read webscrapped training labels
#' @param fname path to webscrapped training labels data
#' @return webscrapped training labels
get_webscrapped_trainingLabels <- function(fname = "webscrapped_trainingLabels.Rds"){
	webscrapped_trainingLabels <- readRDS("webscrapped_trainingLabels.Rds")
	return(webscrapped_trainingLabels)
}

#' read document-term matrix created by the text mining code
#' @param dtm_file path to saved document-term matrix
#' @return document-term matrix
get_DTM <- function(dtm_file= "F:/hguillon/research/exploitation/R/latin_america/data/obj_dtm_from_dfm_country.Rds"){
	obj_dtm <- readRDS(dtm_file)
	return(obj_dtm)
}

#' identifies the subset of paper with validation data and align databases
#' @param titleInd cross-walked indices between human-reading database and topic model
#' @param validationHumanReading human-reading database
#' @param topicDocs results from the topic model
#' @param DTM document-term matrix derived from topic modelled corpus
#' @return list with four elements: titleInd, validationHumanReading, validationTopicDocs, validationDTM
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
	if (scale_type == "temporal"){
		validationHumanReading <- do.call(data.frame, lapply(validationHumanReading, function(x) as.character(x))) 
		validationHumanReading <- do.call(data.frame, lapply(validationHumanReading, function(x) replace(x, which(!x %in% c("0", "1")), "0"))) 
		validationHumanReading <- do.call(data.frame, lapply(validationHumanReading, function(x) as.logical(as.integer(as.character(x))))) 
	}

	# changing colnames
	colnames(validationTopicDocs) <- paste0("Topic", seq(ncol(validationTopicDocs)))

	validationDTM <- as.matrix(validationDTM)
	colnames(validationDTM) <- paste0("Term", seq(ncol(validationDTM)))
	res <- list(validationHumanReading = validationHumanReading,
		validationTopicDocs = validationTopicDocs,
		validationDTM = validationDTM) 
	return(res)
}

