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
	plot_estimates,
	get_n_players,
	assign_articles_to_players,
	update_database
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
load(file = file.path(root.dir, out.dir, paste0("language_dfs_updated_2", ".rda")))

# 2. get number of samples
baseline_pct <- table(language_dfs$english$collected)[["in corpus"]] / nrow(language_dfs$english)
n_spanish <- ceiling(baseline_pct * nrow(language_dfs$spanish)) - table(language_dfs$spanish$collected)[["in corpus"]]
n_portuguese <- ceiling(baseline_pct * nrow(language_dfs$portuguese)) - table(language_dfs$portuguese$collected)[["in corpus"]]
# n <- list(spanish = n_spanish, portuguese = n_portuguese)
n <- list(spanish = 1.2 * n_spanish, portuguese = 1.2 * n_portuguese)
N <- sum(unlist(n))

# 3. get estimates
plot_estimates()

# 4. get samples 
spanish_samples <- get_samples("spanish", n_spanish, pl = TRUE)
portuguese_samples <- get_samples("portuguese", n_portuguese, pl = TRUE)
samples <- list(spanish = spanish_samples, portuguese = portuguese_samples)
ind_spanish <- which(spanish_samples == 1)
ind_portuguese <- which(portuguese_samples == 1)
ind <- list(spanish = ind_spanish, portuguese = ind_portuguese)

# 5. assign urls to downloaders
n_players <- get_n_players()
# n_players <- 12
assign_articles_to_players("spanish", number_of_players = n_players)
assign_articles_to_players("portuguese", number_of_players = n_players)

# 6. update the databases
# language_dfs <- update_database("portuguese", language_dfs)
# language_dfs <- update_database("spanish", language_dfs)
# save(language_dfs, file = file.path(root.dir, out.dir, paste0("language_dfs_updated_2", ".rda")))