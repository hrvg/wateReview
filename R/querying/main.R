#####################
##### LIBRARIES #####
#####################

#################
##### UTILS #####
#################

import::here(.from = "./R/utils/envpath.R", 
	get_rootdir)

import::here(.from = "./R/utils/lib_query.R",
	get_csv_files,
	get_citation_dataframe)

#######################
####### HELPERS #######
#######################

import::here(.from = "./R/querying/helpers.R",
	check_duplicate_row,
	check_duplicate_title,
	write_citation_dataframe)

########################
##### INITIALIZING #####
########################

root.dir <- get_rootdir()

################
##### MAIN #####
################

csv.dir <- "data/latin_america/corpus_csv/spanish/"
csv.dir <- "data/latin_america/corpus_csv/portuguese/"
csv.dir <- "data/latin_america/corpus_csv/english/"

# if(!exists("csv.files")){
	csv.files <- get_csv_files(root.dir, csv.dir)
# }
# if(!exists("citation_dataframe")){
	citation_dataframe <- get_citation_dataframe(csv.files)
# }
citation_dataframe <- check_duplicate_row(citation_dataframe)
citation_dataframe <- check_duplicate_title(citation_dataframe)

write_citation_dataframe(root.dir, csv.dir)