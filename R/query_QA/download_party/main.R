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
load(file = file.path(root.dir, out.dir, paste0("language_dfs", ".rda")))

baseline_pct <- table(language_dfs$english$collected)[["in corpus"]] / nrow(language_dfs$english)

n_spanish <- ceiling(baseline_pct * nrow(language_dfs$spanish)) - table(language_dfs$spanish$collected)[["in corpus"]]
n_portuguese <- ceiling(baseline_pct * nrow(language_dfs$portuguese)) - table(language_dfs$portuguese$collected)[["in corpus"]]
N <- n_spanish + n_portuguese

print_estimate <- function(n_dl = 8, time = 5){
	print("Estimate (h)")
	print(signif(N * time / 60 / n_dl, 3))
	return(N * time / 60 / n_dl)
}

if (!require("oce")) install.packages("oce")
library("oce")

downloaders <- seq(7, 7+5)	
times <- seq(1, 5, 0.5)	
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


# adjusting for time
par(mfrow = c(2, 1))
hist(language_dfs[[language]]$Year, xlab = "Year", main = "in query") 
hist(language_dfs[[language]]$Year[language_dfs[[language]]$collected == "in corpus"], xlab = "Year", main = "in corpus")

pt_query <- prop.table(table(language_dfs[[language]]$Year))
.pt_corpus <- prop.table(table(language_dfs[[language]]$Year[language_dfs[[language]]$collected == "in corpus"]))
pt_corpus <- rep(0, length(pt_query))
pt_corpus[match(names(.pt_corpus), names(pt_query))] <- .pt_corpus

pt_df_year <- data.frame(
	year = as.numeric(names(pt_query)),
	query = as.vector(pt_query),
	corpus = pt_corpus)

pt_df_year$resampling <- pt_query - pt_corpus
pt_df_year$resampling[pt_df_year$resampling < 0] <- 0
pt_df_year$resampling <- pt_df_year$resampling / sum(pt_df_year$resampling)


# adjusting for journals
pt_query <- prop.table(table(language_dfs[[language]]$Source))
.pt_corpus <- prop.table(table(language_dfs[[language]]$Source[language_dfs[[language]]$collected == "in corpus"]))
pt_corpus <- rep(0, length(pt_query))
pt_corpus[match(names(.pt_corpus), names(pt_query))] <- .pt_corpus

pt_df_source <- data.frame(
	source = names(pt_query),
	query = as.vector(pt_query),
	corpus = pt_corpus)
pt_df_source$resampling <- pt_query - pt_corpus
pt_df_source$resampling[pt_df_source$resampling < 0] <- 0
pt_df_source$resampling <- pt_df_source$resampling / sum(pt_df_source$resampling)

# get probabilities
prob <- pt_df_source$resampling[match(language_dfs[[language]]$Source, pt_df_source$source)] * pt_df_year$resampling[match(language_dfs[[language]]$Year, pt_df_year$year)]
prob[language_dfs[[language]]$collected == "in corpus"] <- 0
prob[prob == 0] <- 1E-16
prob <- prob / sum(prob)

set.seed(1789)
pik <- inclusionprobabilities(prob, n[[language]])
samples <- sampling::UPsystematic(pik)

# checking for years
pt_query <- prop.table(table(language_dfs[[language]]$Year))
.pt_corpus <- prop.table(table(language_dfs[[language]]$Year[language_dfs[[language]]$collected == "in corpus"]))
pt_corpus <- rep(0, length(pt_query))
pt_corpus[match(names(.pt_corpus), names(pt_query))] <- .pt_corpus

.pt_resampled <- prop.table(table(language_dfs[[language]]$Year[samples == 1 | language_dfs[[language]]$collected == "in corpus"]))
pt_resampled <- rep(0, length(pt_query))
pt_resampled[match(names(.pt_resampled), names(pt_query))] <- .pt_resampled


pt_df_year <- data.frame(
	year = as.numeric(names(pt_query)),
	query = as.vector(pt_query),
	corpus = pt_corpus,
	resampled = pt_resampled)

pt_df_year$resampling <- pt_query - pt_corpus
pt_df_year$resampling[pt_df_year$resampling < 0] <- 0
pt_df_year$resampling <- pt_df_year$resampling / sum(pt_df_year$resampling)

par(mfrow = c(1,1))
plot(pt_df_year$year, pt_df_year$query, lty = 2, col = "blue", ylim = c(0, 1.05 * max(max(pt_df_year$query), max(pt_df_year$corpus), max(pt_df_year$resampling))), ylab = "Frequency", xlab = "Year")
points(pt_df_year$year, pt_df_year$corpus, col = "red")
points(pt_df_year$year, pt_df_year$resampling, col = "purple")
points(pt_df_year$year, pt_df_year$resampled, col = "black", pch = 3)


# checking for journals
pt_query <- prop.table(table(language_dfs[[language]]$Source))
.pt_corpus <- prop.table(table(language_dfs[[language]]$Source[language_dfs[[language]]$collected == "in corpus"]))
pt_corpus <- rep(0, length(pt_query))
pt_corpus[match(names(.pt_corpus), names(pt_query))] <- .pt_corpus

.pt_resampled <- prop.table(table(language_dfs[[language]]$Source[samples == 1 | language_dfs[[language]]$collected == "in corpus"]))
pt_resampled <- rep(0, length(pt_query))
pt_resampled[match(names(.pt_resampled), names(pt_query))] <- .pt_resampled

pt_df_source <- data.frame(
	source = names(pt_query),
	query = as.vector(pt_query),
	corpus = pt_corpus,
	resampled = pt_resampled)

pt_df_source$resampling <- pt_query - pt_corpus
pt_df_source$resampling[pt_df_source$resampling < 0] <- 0
pt_df_source$resampling <- pt_df_source$resampling / sum(pt_df_source$resampling)


par(mfrow = c(1,1))
plot(seq(nrow(pt_df_source)), pt_df_source$query, lty = 2, col = "blue", ylim = c(0, 1.05 * max(max(pt_df_source$query), max(pt_df_source$corpus), max(pt_df_source$resampling))), ylab = "Frequency", xlab = "Source")
points(seq(nrow(pt_df_source)), pt_df_source$corpus, col = "red")
points(seq(nrow(pt_df_source)), pt_df_source$resampling, col = "purple")
points(seq(nrow(pt_df_source)), pt_df_source$resampling, col = "black", pch = 3)