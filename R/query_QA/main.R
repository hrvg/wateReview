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
# language_dfs <- get_language_dfs(languages)
load(file = file.path(root.dir, out.dir, paste0("language_dfs", ".rda")))
meta_df <- get_meta_df(language_dfs)

# 2. graphs
pp <- lapply(languages, function(language) source_plot(language, n = 30, facet = FALSE))
pps <- lapply(languages, function(language) source_plot(language, n = 30, facet = TRUE))

p2 <- ggplot(meta_df, aes(x = Year, group = language, fill = language)) +
	geom_bar(aes(y = (..count..)/sum(..count..))) + 
	scale_y_continuous(labels=scales::percent) +
	ylab("relative frequencies") +
	facet_grid(~collected)

p3 <- ggplot(meta_df, aes(x = Year, fill = language)) +
	geom_density(alpha = .25, bw = 1) +
	facet_grid(~collected)

p5 <- ggplot(meta_df[meta_df$Cites >= 1, ], aes(y = Cites, x = language)) +
	geom_violin(aes(color = collected), trim = FALSE, adjust = 1, alpha = 0.5) +
	geom_boxplot(aes(fill = collected), position = position_dodge(preserve = "total", width = 0.9 ), width = 0.1) +
 	geom_text(data = generate_label_df(TukeyHSD(aov(Cites ~ language, data = meta_df), ordered = FALSE, conf.level = .95),
 		"language", meta_df, "Cites", fact = 2, yadjust = 100),
 		 aes(x = match(plot.labels, languages) - 0.22, y = V1, label = labels)) +
 	geom_text(data = generate_label_df(TukeyHSD(aov(Cites ~ language, data = meta_df[meta_df$collected == "in corpus", ]), ordered = FALSE, conf.level = .95),
	"language", meta_df[meta_df$collected == "in corpus", ], "Cites", fact = 2, yadjust = 100),
	 aes(x = match(plot.labels, languages) + 0.22, y = V1, label = labels)) +
	scale_y_log10() + 
	ylab("citations") + 
	xlab("language") 

plot.list <- list(pp, pps, p2, p3, p5)

pdf(file.path(root.dir, out.dir, "query_QA.pdf"), width = 17, height = 11, title = "query_QA")
print(plot.list)
dev.off()