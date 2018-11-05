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


import::here(.from = "./R/utils/lib_query.R",
	get_meta_df,
	get_language_dfs,
	make_pretty_str
	)

#######################
####### HELPERS #######
#######################

import::here(.from = "./R/EndNote_processing/helpers.R", 
	get_endnote_titles,
	get_endnote_xml)

##############
#### INIT ####
##############

root.dir <- get_rootdir()
out.dir <- "exploitation/out/run79"
languages <- c("english", "portuguese", "spanish")

##############
#### MAIN ####
##############

### FOREWORD ###
# The time spent scripting blew up (10+ hours) because EndNote formats everything to its own liking, making comparison with the files coming from the query process more difficult.
# Here, the EndNote database is useful because it holds the location of the .pdf files for each of the articles that have been retrieved.
# It also holds the various urls of the article.
# We need that information.
# 
# However, because of text formating issues, EndNote data has to be exported as .xml and as .txt. Both files have the same ordering, but the .xml is encoded as UTF-8 whereas the .txt is encoded as Windows 1252 to allow comparison with the .csv coming from the query process.
# Basically, in the following, the .txt is used to align the .xml with .csv

# 1. read csv databases
language_dfs <- get_language_dfs(languages)
meta_df <- get_meta_df(language_dfs)

# 2. align EndNote .xml database with query .csv database
for (language in languages){
	print(language)
	# xmldf <- get_endnote_xml(language)
	# endnote_titles <- get_endnote_titles(language)

	# endnote_titles <- make_pretty_str(endnote_titles)
	# query_titles <- make_pretty_str(language_dfs[[language]]$Title)

	# stopifnot(all(query_titles %in% endnote_titles) & all(endnote_titles %in% query_titles))

	# language_dfs[[language]] <- cbind(language_dfs[[language]][match(endnote_titles, query_titles), ], xmldf)

	# stopifnot(names(table(language_dfs[[language]]$`electronic-resource-num` == language_dfs[[language]]$DOI)) == "TRUE")

	collected <- ifelse(is.na(language_dfs[[language]]$pdfs), "absent from corpus", "in corpus")
	language_dfs[[language]]$collected
}

save(language_dfs, file = file.path(root.dir, out.dir, paste0("language_dfs", ".rda")))