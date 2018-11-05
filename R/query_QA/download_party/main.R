#####################
##### LIBRARIES #####
#####################

library(ggplot2)
library(forcats)
library(plyr)
library(dplyr)
library(cowplot)
library(gridExtra)
library(multcompView)

if (!require("oce")) install.packages("oce")
library("oce")

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

import::here(.from = "./R/query_QA/helpers.R", 
	get_language,
	read_citation_dataframe,
	source_plot,
	generate_label_df
	)

import::here(.from = "./R/query_QA/download_party/helpers.R", 
	get_samples,
	print_estimate,
	plot_estimates
	)

##############
#### INIT ####
##############

root.dir <- get_rootdir()
languages <- c("english", "portuguese", "spanish")
out.dir <- "exploitation/out/run79"

##############
#### MAIN ####
##############

# 1. read csv databases
load(file = file.path(root.dir, out.dir, paste0("language_dfs", ".rda")))

# 2. get number of samples
baseline_pct <- table(language_dfs$english$collected)[["in corpus"]] / nrow(language_dfs$english)
n_spanish <- ceiling(baseline_pct * nrow(language_dfs$spanish)) - table(language_dfs$spanish$collected)[["in corpus"]]
n_portuguese <- ceiling(baseline_pct * nrow(language_dfs$portuguese)) - table(language_dfs$portuguese$collected)[["in corpus"]]
N <- n_spanish + n_portuguese

# 3. get estimates
plot_estimates()

# 4. get samples
spanish_samples <- get_samples("spanish", n_spanish, pl = TRUE)
portuguese_samples <- get_samples("portuguese", n_portuguese, pl = TRUE)