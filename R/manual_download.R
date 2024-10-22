
#' Print the time estimate for manual downloading
#' @param n_dl numeric, number of downloaders
#' @param time numeric, time to download one document in minutes
#' @param pr logical, controls the output of the values of the estimated times
#' @return The time estimates in hours
#' @export 
print_estimate <- function(n_dl = 8, time = 5, pr = FALSE){
	if (pr){
		print("Estimate (h)")
		print(signif(N * time / 60 / n_dl, 3))
	} 
	return(N * time / 60 / n_dl)
}

#' Plot a time estimate matrix for a different number of downlaoders
#' @param downloaders numeric, vector of number of downloaders
#' @param times numeric, vector of times to download one article
#' @return a heat map plot
#' @export
plot_estimates <- function(downloaders = seq(7, 7+5), times = seq(1, 5, 0.5)){
	estimates <- outer(downloaders, times, print_estimate)
	p <- oce::imagep(
		x = downloaders,
		y = times,
		z = estimates,
		col = oce::oce.colorsJet(14),
		ylab = 'Time [min]',
		xlab = '# downloaders [-]',
		zlab = 'Time [h]',
		drawPalette = TRUE,
		decimate = FALSE)
}

#' Select the documents for downloading while correcting for bias in terms of year and sources
#' @param language character, one of "enlish", "portuguese", "spanish"
#' @param n the number of samples to select
#' @param pl logical, controls if a plot is produced or not
#' @return samples indices
#' @export
get_samples <- function(language, n, pl = FALSE){

	get_pt_df <- function(var, resampled = FALSE){
		pt_query <- prop.table(table(language_dfs[[language]][[var]]))
		.pt_corpus <- prop.table(table(language_dfs[[language]][[var]][language_dfs[[language]]$collected == "in corpus"]))
		pt_corpus <- rep(0, length(pt_query))
		pt_corpus[match(names(.pt_corpus), names(pt_query))] <- .pt_corpus

		pt_df_var <- data.frame(
			var = if (var =="Year") as.numeric(names(pt_query)) else names(pt_query),
			query = as.vector(pt_query),
			corpus = pt_corpus)
		pt_df_var$resampling <- pt_query - pt_corpus
		pt_df_var$resampling[pt_df_var$resampling < 0] <- 0
		pt_df_var$resampling <- pt_df_var$resampling / sum(pt_df_var$resampling)
		names(pt_df_var) <- c(tolower(var), names(pt_df_var)[-1])
		if (resampled){
			.pt_resampled <- prop.table(table(language_dfs[[language]][[var]][samples == 1 | language_dfs[[language]]$collected == "in corpus"]))
			pt_resampled <- rep(0, length(pt_query))
			pt_resampled[match(names(.pt_resampled), names(pt_query))] <- .pt_resampled
			pt_df_var$resampled <- pt_resampled
		}
		return(pt_df_var)
	}

	pt_df_year <- get_pt_df("Year")
	pt_df_source <- get_pt_df("Source")

	# get probabilities
	prob <- pt_df_source$resampling[match(language_dfs[[language]]$Source, pt_df_source$source)] * pt_df_year$resampling[match(language_dfs[[language]]$Year, pt_df_year$year)]
	prob[language_dfs[[language]]$collected == "in corpus"] <- 0
	prob[prob == 0] <- 1E-16
	prob <- prob / sum(prob)

	set.seed(1789)
	pik <- sampling::inclusionprobabilities(prob, n)
	samples <- sampling::UPsystematic(pik)

	if (pl){
		# checking for years
		pt_df_year <- get_pt_df("Year", resampled = TRUE)
			par(mfrow = c(1,1))
		plot(pt_df_year$year, pt_df_year$query, col = "blue", ylim = c(0, 1.05 * max(max(pt_df_year$query), max(pt_df_year$corpus), max(pt_df_year$resampling))), ylab = "Frequency", xlab = "Year")
		points(pt_df_year$year, pt_df_year$corpus, col = "red")
		points(pt_df_year$year, pt_df_year$resampling, col = "purple")
		points(pt_df_year$year, pt_df_year$resampled, col = "black", pch = 3)
		legend(x="topleft", legend = c("Query", "Corpus", "Resampling", "Corpus after resampling"), col = c("blue", "red", "purple", "black"), pch = c(1, 1, 1, 3), bty = "n")


		pt_df_source <- get_pt_df("Source", resampled = TRUE)
		par(mfrow = c(1,1))
		plot(seq(nrow(pt_df_source)), pt_df_source$query, col = "blue", ylim = c(0, 1.05 * max(max(pt_df_source$query), max(pt_df_source$corpus), max(pt_df_source$resampling))), ylab = "Frequency", xlab = "Source")
		points(seq(nrow(pt_df_source)), pt_df_source$corpus, col = "red")
		points(seq(nrow(pt_df_source)), pt_df_source$resampling, col = "purple")
		points(seq(nrow(pt_df_source)), pt_df_source$resampled, col = "black", pch = 3)
		legend(x="topright", legend = c("Query", "Corpus", "Resampling", "Corpus after resampling"), col = c("blue", "red", "purple", "black"), pch = c(1, 1, 1, 3), bty = "n")
	}

	return(samples)
}

#' Create a `.csv` files with the information to download the documents
#' @param language character, one of "english", "spanish", "portuguese"
#' @param number_of_players numeric, number of downloarders
#' @export 
assign_articles_to_players <- function(language, number_of_players = 8){
	selected <- language_dfs[[language]][ind[[language]], ]
	selected <- selected[, colnames(selected) %in% c("Authors", "Title", "Year", "Source", "urls", "DOI")]
	selected$ID <- ind[[language]]
	selected <- selected[order(selected$Source), ]
	selected <- selected[, c(7, 2, 1, 3, 4, 6, 5)]
	chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
	chunked <- chunk2(seq(nrow(selected)), number_of_players)
	dir.create(file.path(root.dir, out.dir, language))
	for (i in 1:number_of_players){
		dir.create(file.path(root.dir, out.dir, language, paste0("player_", i)))
		ind <- chunked[[i]]
		write.csv(selected[ind, ], file.path(root.dir, out.dir, language, paste0("player_", i), paste0("to_dl.csv")), row.names = FALSE)
	}

}

#' Prompt to get the number of players
#' @export
get_n_players <- function(){
	n_players <- readline(prompt="How many people are downloading?: ")
	return( as.numeric(n_players) )
}

#' Update the database by marking the manuall downloaded articles as `in corpus`
#' @param language character, one of "english", "spanish", "portuguese"
#' @param language_dfs list of data.frame with the information from the query and the corpus collection
#' @return udpated `language_dfs` list of data.frame with the information from the query and the corpus collection
#' @export
update_database <- function(language, language_dfs){
	language_ld <- list.dirs(file.path(root.dir, paste0("data/latin_america/corpus_pdf/", language, "/manual_download_", language)), recursive = TRUE, full.names = FALSE)[-1]
	language_recovery_rate <- length(language_ld) / n$language
	for (d in as.numeric(language_ld)){
		if (is.na(language_dfs[[language]]$pdfs[d])){
			f <- list.files(file.path(root.dir, paste0("data/latin_america/corpus_pdf/", language, "/manual_download_", language), d))
			language_dfs[[language]]$pdfs[d] <- paste0("manual_download_", language, "/", d, "/", f)
			language_dfs[[language]]$collected[d] <- "in corpus"
		}
	}
	return(language_dfs)
}

filter_database <- function(language, language_dfs){
	df <- language_dfs[[language]]
	return(df[which(!is.na(df$pdfs)), ])
}