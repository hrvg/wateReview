get_language <- function(){
	language <- readline(prompt="Select a language [english, spanish, portuguese]: ")
	return( as.character(language) )
}

read_citation_dataframe <- function(language){
	csv.dir <- paste0("data/latin_america/corpus_csv/", language, "/")
	csv.file <- 'citation_dataframe.csv'
	citation.df <- read.csv(file.path(root.dir, csv.dir, csv.file), header = TRUE)
	citation.df$language <- language
	csv.file <- paste0(language, '_title.txt')
	# retrieved.df <- unname(unlist(read.csv(file.path(root.dir, csv.dir, csv.file), header = FALSE, sep="\t")))
	retrieved.df <- readLines(file.path(root.dir, csv.dir, csv.file))
	citation.df$collected <- make_pretty_str(citation.df$Title) %in% make_pretty_str(retrieved.df)
	citation.df$collected <- ifelse(citation.df$collected, "in corpus", "absent from corpus")
	citation.df$DOI_flag <- ifelse(citation.df$DOI %in% c("", "<NA>"), "DOI absent", "DOI present")
	return(citation.df)
}

source_plot <- function(language, n = 40, facet = TRUE){
	citation.df <- meta_df[meta_df$language == language, ]
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

