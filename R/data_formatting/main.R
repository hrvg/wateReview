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

pdf.dir <- "data/latin_america/corpus_pdf/spanish/spanish.Data/"
pdf.dir <- "data/latin_america/corpus_pdf/portuguese/portuguese.Data/"
pdf.dir <- "data/latin_america/corpus_pdf/english/english.Data/"

out.dir <- "data/latin_america/corpus_pdf/spanish/"
out.dir <- "data/latin_america/corpus_pdf/portuguese/"
out.dir <- "data/latin_america/corpus_pdf/english/"

l <- get_pdf_files(root.dir, pdf.dir)
names <- unname(l$names)
full.names <- l$full.names

non_duplicate_index <- get_duplicate_pdfs(names)

select_ind <- article_selection(full.names, non_duplicate_index, ratio = 0.2) 

number_of_readers <- 4

assign_articles_to_readers(select_ind, number_of_readers, out.dir)