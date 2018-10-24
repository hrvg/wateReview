#####################
##### LIBRARIES #####
#####################

library(ggplot2)
library(forcats)
library(plyr)
library(dplyr)
library(cowplot)
library(gridExtra)

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

root.dir = get_rootdir()



# 1. read csv database

get_language <- function(){
	language <- readline(prompt="Select a language [english, spanish, portuguese]: ")
	return( as.character(language) )
}

# language <- get_language()
languages <- c("english", "portuguese", "spanish")

dfs <- lapply(languages, function(language){
	csv.dir <- paste0("data/latin_america/corpus_csv/", language, "/")
	csv.file <- 'citation_dataframe.csv'
	citation.df <- read.csv(file.path(root.dir, csv.dir, csv.file), header = TRUE)
	citation.df$language <- language
	csv.file <- paste0(language, '_title.csv')
	retrieved.df <- unname(unlist(read.csv(file.path(root.dir, csv.dir, csv.file), header = FALSE, sep="\t")))
	citation.df$collected <- citation.df$Title %in% retrieved.df
	citation.df$collected <- ifelse(citation.df$collected, "in corpus", "absent from corpus")
	citation.df$DOI_flag <- ifelse(citation.df$DOI %in% c("", "<NA>"), "DOI absent", "DOI present")
	return(citation.df)
})

meta.df <- do.call(rbind, dfs)


# 2. get fields Source, Year, Cites

source_plot <- function(language, n = 40, facet = TRUE){
	citation.df <- meta.df[meta.df$language == language, ]
	citation.df$Source <- tolower(citation.df$Source)
	tab <- table(citation.df$Source)
	citation.df$Source <- factor(citation.df$Source, levels = names(tab)[order(-tab)])
	p1 <- ggplot(citation.df[citation.df$Source %in% head(names(tab)[order(-tab)], n), ], aes(x = Source, group = collected, fill = collected)) +
		geom_bar() +
		# theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
		scale_x_discrete(limits = tail(rev(levels(citation.df$Source)), n)) +
		coord_flip() +
		ylab("count")
	if (facet){
		p1 <- p1 + facet_wrap(DOI_flag~collected)
	}	
	return(p1)	
}

pp <- lapply(languages, function(language) source_plot(language, n = 30, facet = FALSE))

pps <- lapply(languages, function(language) source_plot(language, n = 30, facet = TRUE))

p2 <- ggplot(meta.df, aes(x = Year, group = language, fill = language)) +
	geom_bar(aes(y = (..count..)/sum(..count..))) + 
	scale_y_continuous(labels=scales::percent) +
	ylab("relative frequencies") +
	facet_grid(~collected)

p3 <- ggplot(meta.df, aes(x = Year, fill = language)) +
	geom_density(alpha = .25, bw = 1) +
	facet_grid(~collected)

# p4 <- ggplot(meta.df[meta.df$Cites >= 1, ], aes(x = Cites, group = language, fill = language)) +
# 	geom_density(alpha = .3, bw = .1) +
# 	scale_x_log10() + 
# 	facet_grid(~collected) +
# 	xlab("citations")

# p5 <- ggplot(meta.df[meta.df$Cites >= 1 & meta.df$Year > 2000, ], aes(x = Cites, fill = language)) +
# 	geom_density(alpha = .3, bw = .1) +
# 	scale_x_log10()	

generate_label_df <- function(HSD, flev, d, y, fact = 1, yadjust = 0){
 # Extract labels and factor levels from Tukey post-hoc 
 Tukey.levels <- HSD[[flev]][,4]
 Tukey.labels <- multcompLetters(Tukey.levels)['Letters']
 plot.labels <- names(Tukey.labels[['Letters']])
 # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
 # upper quantile and label placement
    boxplot.df <- ddply(d, flev, function (x) (max(fivenum(x[[y]])) * fact) + yadjust)

 # Create a data frame out of the factor levels and Tukey's homogenous group letters
  plot.levels <- data.frame(plot.labels, labels = Tukey.labels[['Letters']],
     stringsAsFactors = FALSE)

 # Merge it with the labels
   labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)

return(labels.df)
}


p5 <- ggplot(meta.df[meta.df$Cites >= 1, ], aes(y = Cites, x = language)) +
	geom_violin(aes(color = collected), trim = FALSE, adjust = 1, alpha = 0.5) +
	geom_boxplot(aes(fill = collected), position = position_dodge(preserve = "total", width = 0.9 ), width = 0.1) +
 	geom_text(data = generate_label_df(TukeyHSD(aov(Cites ~ language, data = meta.df), ordered = FALSE, conf.level = .95),
 		"language", meta.df, "Cites", fact = 2, yadjust = 100),
 		 aes(x = match(plot.labels, languages) - 0.22, y = V1, label = labels)) +
 	geom_text(data = generate_label_df(TukeyHSD(aov(Cites ~ language, data = meta.df[meta.df$collected == "in corpus", ]), ordered = FALSE, conf.level = .95),
	"language", meta.df[meta.df$collected == "in corpus", ], "Cites", fact = 2, yadjust = 100),
	 aes(x = match(plot.labels, languages) + 0.22, y = V1, label = labels)) +
	scale_y_log10() + 
	ylab("citations") + 
	xlab("language") 
p5

plot.list <- list(pp, pps, p2, p3, p5)

out.dir <- "exploitation/out/run79"
pdf(file.path(root.dir, out.dir, "query_QA.pdf"), width = 17, height = 11, title = "query_QA")
print(plot.list)
dev.off()