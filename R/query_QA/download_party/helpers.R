print_estimate <- function(n_dl = 8, time = 5){
	print("Estimate (h)")
	print(signif(N * time / 60 / n_dl, 3))
	return(N * time / 60 / n_dl)
}

plot_estimates <- function(downloaders = seq(7, 7+5), times = seq(1, 5, 0.5)){
	estimates <- outer(downloaders, times, print_estimate)
	p <- oce::imagep(
		x = downloaders,
		y = times,
		z = estimates,
		col = oce.colorsJet(14),
		ylab = 'Time [min]',
		xlab = '# downloaders [-]',
		zlab = 'Time [h]',
		drawPalette = TRUE,
		decimate = FALSE)
}



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
	pik <- inclusionprobabilities(prob, n)
	samples <- sampling::UPsystematic(pik)

	if (pl){
		# checking for years
		pt_df_year <- get_pt_df("Year", resampled = TRUE)
			par(mfrow = c(1,1))
		plot(pt_df_year$year, pt_df_year$query, col = "blue", ylim = c(0, 1.05 * max(max(pt_df_year$query), max(pt_df_year$corpus), max(pt_df_year$resampling))), ylab = "Frequency", xlab = "Year")
		points(pt_df_year$year, pt_df_year$corpus, col = "red")
		points(pt_df_year$year, pt_df_year$resampling, col = "purple")
		points(pt_df_year$year, pt_df_year$resampled, col = "black", pch = 3)

		pt_df_source <- get_pt_df("Source", resampled = TRUE)
		par(mfrow = c(1,1))
		plot(seq(nrow(pt_df_source)), pt_df_source$query, col = "blue", ylim = c(0, 1.05 * max(max(pt_df_source$query), max(pt_df_source$corpus), max(pt_df_source$resampling))), ylab = "Frequency", xlab = "Source")
		points(seq(nrow(pt_df_source)), pt_df_source$corpus, col = "red")
		points(seq(nrow(pt_df_source)), pt_df_source$resampling, col = "purple")
		points(seq(nrow(pt_df_source)), pt_df_source$resampled, col = "black", pch = 3)
		
	}

	return(samples)
}
