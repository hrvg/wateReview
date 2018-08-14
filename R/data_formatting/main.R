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

################
##### MAIN #####
################

pdf.dir <- "data/latin_america/corpus_pdf/portuguese/portuguese.Data/"
pdf.dir <- "data/latin_america/corpus_pdf/english/english.Data/"
pdf.dir <- "data/latin_america/corpus_pdf/spanish/spanish.Data/"

out.dir <- "data/latin_america/corpus_pdf/portuguese/"
out.dir <- "data/latin_america/corpus_pdf/english/"
out.dir <- "data/latin_america/corpus_pdf/spanish/"

l <- get_pdf_files(root.dir, pdf.dir)
names <- unname(l$names)
full.names <- l$full.names

duplicate_index <- get_duplicate_pdfs(names)

select_ind <- article_selection(full.names, duplicate_index)

number_of_readers <- 3

assign_articles_to_readers(select_ind, number_of_readers, out.dir)