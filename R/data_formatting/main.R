#####################
##### LIBRARIES #####
#####################

#################
##### UTILS #####
#################

import::here(.from = "./R/utils/envpath.R", 
	get_rootdir)

#######################
####### HELPERS #######
#######################

import::here(.from = "./R/data_formatting/helpers.R",
	get_pdf_files,
	get_duplicate_pdfs,
	article_selection,
	assign_articles_to_readers)

########################
##### INITIALIZING #####
########################

root.dir <- get_rootdir()
languages <- c("english", "portuguese", "spanish")
out.dir <- "exploitation/out/run79"

################
##### MAIN #####
################

pdf.dir <- "data/latin_america/corpus_pdf/spanish/spanish.Data/"
pdf.dir <- "data/latin_america/corpus_pdf/portuguese/portuguese.Data/"
pdf.dir <- "data/latin_america/corpus_pdf/english/english.Data/"

out.dir <- "data/latin_america/corpus_pdf/spanish/"
out.dir <- "data/latin_america/corpus_pdf/portuguese/"
out.dir <- "data/latin_america/corpus_pdf"

l <- get_pdf_files(root.dir, pdf.dir)
full.names <- unname(l$full.names)
full.full.names <- l$full.full.names

# 1. read csv databases
load(file = file.path(root.dir, out.dir, paste0("language_dfs_updated_2", ".rda")))

# lang <- "portuguese"
lang <- "spanish"
# lang <-  "english"
full.names <- language_dfs[[lang]]$pdfs
full.names <- full.names[!is.na(full.names)]

if (lang %in% c("spanish", "portuguese")){
	ind <- grep("manual_download", full.names)
	full.names[-ind] <- paste0(lang, '.Data/PDF/', full.names[-ind])
} else {
	full.names <- paste0(lang, '.Data/PDF/', full.names)
}

names <- unlist(lapply(full.names, function(name){
	tail(strsplit(name, "/")[[1]], 1)
}))

if (lang == "english"){
	read_articles <- list.files(file.path(root.dir, out.dir, lang, "read_articles"), recursive = TRUE)
	read_articles <- unlist(lapply(read_articles, function(name){
		tail(strsplit(name, "/")[[1]], 1)
	}))
	read_articles <- unlist(lapply(read_articles, function(name){
		strsplit(name, "_")[[1]][-1]
	}))
	read_articles <- paste0(read_articles, ".pdf")
	ind <- which(names %in% read_articles)
	names <- names[-ind]
	full.names <- full.names[-ind]
}


non_duplicate_index <- get_duplicate_pdfs(names)

select_ind <- article_selection(full.names, non_duplicate_index, ratio = 0.2) 

number_of_readers <- 3

assign_articles_to_readers(select_ind, number_of_readers, paste0(out.dir, "/", lang))