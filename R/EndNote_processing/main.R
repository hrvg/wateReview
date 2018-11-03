#####################
##### LIBRARIES #####
#####################

library(ggplot2)
library(forcats)
library(plyr)
library(dplyr)
library(cowplot)
library(gridExtra)
library(XML)

#################
##### UTILS #####
#################

import::here(.from = "./R/utils/envpath.R", 
	get_rootdir)

#######################
####### HELPERS #######
#######################

##############
#### MAIN ####
##############

### INIT ###
root.dir = get_rootdir()
language <- "spanish"
pdf.dir <- paste0("data/latin_america/corpus_pdf/", language)

doc <- xmlParse(file.path(root.dir, pdf.dir, paste0(language, "_database.xml")))
xmldf <- xmlToDataFrame(nodes = getNodeSet(doc, "//record"))

urls <- sapply(xmldf$urls, function(url){
	unlist(strsplit(url, "internal-pdf://"))[1]
})

pdfs <- unname(sapply(xmldf$urls, function(url){
	unlist(strsplit(url, "internal-pdf://"))[2]
}))

xmldf$pdfs <- pdfs
xmldf$dates <- as.numeric(sub("//", "", xmldf$dates))

which(xmldf$titles %in% dfs[[3]]$Title)

authors <- sapply(dfs[[3]]$Authors, function(authors){
	str <- unlist(strsplit(as.character(authors), " "))
	if (grepl(".", str[1])){
		return(str[2])
	} else {
		return(str[1])
	}
})

short_titles <- unname(sapply(xmldf$pdfs, function(pdf){
	tools::file_path_sans_ext(tail((unlist(strsplit(pdf, "-"))), 1))
}))

short_titles <- unname(sapply(retrieved.df, function(pdf){
	substring(pdf, 1, 30)
}))

ind <- lapply(short_titles, function(title){
	if (grepl("\\(", title)) title <- unlist(strsplit(title, "\\("))[1]
	if (grepl("\\[", title)) title <- unlist(strsplit(title, "\\["))[1]
	if (grepl("\\{", title)) title <- unlist(strsplit(title, "\\{"))[1]
	return(grep(title, dfs[[3]]$Title))
})

rr <- gsub("[^[:alnum:][:space:]]","",retrieved.df)