#' Interactive prompt to select language
#' @return character, language
#' @export
get_language <- function(){
	language <- readline(prompt="Select a language [english, spanish, portuguese]: ")
	return( as.character(language) )
}

#' Read the citation data.frame exported by the query processing
#' @param language character, one of "english", "spanish", "portuguese"
#' @return data.frame
#' @export
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

#' Produce a barplot of the different sources present in the query and in the collected corpus
#' @param meta_df data.frame with data of multiple `language`
#' @param language character, one of "english", "spanish", "portuguese"
#' @param n numeric, number of bars to display
#' @param facet logical, controls if the plot is faceted by DOI presence
#' @return `ggplot` object
#' @import ggplot2
#' @export
source_plot <- function(meta_df, language, n = 40, facet = TRUE){
	citation.df <- meta_df[meta_df$language == language, ]
	citation.df$Source <- tolower(citation.df$Source)
	tab <- table(citation.df$Source)
	citation.df$Source <- factor(citation.df$Source, levels = names(tab)[order(-tab)])
	p1 <- ggplot(citation.df[citation.df$Source %in% head(names(tab)[order(-tab)], n), ], aes(x = Source, group = collected, fill = collected)) +
		geom_bar() +
		# theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
		scale_x_discrete(limits = tail(rev(levels(citation.df$Source)), n)) +
		coord_flip() +
		ylab("count") +
		labs(title = language)
	if (facet){
		p1 <- p1 + facet_wrap(DOI_flag~collected)
	}	
	return(p1 + ggpubr::theme_pubr())	
}

#' General a data.frame of labels for a violin plot
#' @param HSD results from  `TukeyHSD()`
#' @param flev `factor` to use for the Tukey levels
#' @param d data.frame
#' @param y second `factor` for the Tukey levels
#' @param fact multiplies the maximum value to adjust graphical position
#' @param yadjust absolute adjustment position
#' @export
#' @return data.frame
generate_label_df <- function(HSD, flev, d, y, fact = 1, yadjust = 0){
 # Extract labels and factor levels from Tukey post-hoc 
 Tukey.levels <- HSD[[flev]][,4]
 Tukey.labels <- multcompView::multcompLetters(Tukey.levels)['Letters']
 plot.labels <- names(Tukey.labels[['Letters']])
 # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
 # upper quantile and label placement
    boxplot.df <- plyr::ddply(d, flev, function (x) (max(fivenum(x[[y]])) * fact) + yadjust)

 # Create a data frame out of the factor levels and Tukey's homogenous group letters
  plot.levels <- data.frame(plot.labels, labels = Tukey.labels[['Letters']],
     stringsAsFactors = FALSE)

 # Merge it with the labels
   labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)

return(labels.df)
}

#' Make QA/QC plot for a given data.frame by comparing corpus present in the query and actually collected
#' @param meta_df a data.frame with information about both the query and the collected corpus
#' @param languages character, vector of possible languages
#' @return a named list with 5 plots: `source`, `source_DOI`, `time_histogram`, `time_density`, `citations`
#' @import ggplot2
#' @export 
query_QA_plots <- function(meta_df, languages = c("english", "portuguese", "spanish")){
	source <- lapply(languages, function(language) source_plot(meta_df, language, n = 30, facet = FALSE))
	source_DOI <- lapply(languages, function(language) source_plot(meta_df, language, n = 30, facet = TRUE))
	names(source) <- names(source_DOI) <- languages

	time_histogram <- ggplot(meta_df, aes(x = Year, group = language, fill = language)) +
		geom_bar(aes(y = (..count..)/sum(..count..))) + 
		scale_y_continuous(labels=scales::percent) +
		ylab("relative frequencies") +
		facet_grid(~collected) +
		ggpubr::theme_pubr()

	time_density <- ggplot(meta_df, aes(x = Year, fill = language)) +
		geom_density(alpha = .25, bw = 1) +
		facet_grid(~collected) +
		ggpubr::theme_pubr()

	citations <- ggplot(meta_df[meta_df$Cites >= 1, ], aes(y = Cites, x = language)) +
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
		xlab("language")  +
		ggpubr::theme_pubr()

	res <- list(source = source, source_DOI = source_DOI, time_histogram = time_histogram, time_density = time_density, citations = citations)
	return(res)
}