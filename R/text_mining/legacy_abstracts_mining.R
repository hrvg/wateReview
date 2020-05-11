# HG 2020-05-11: I am not sure what is the purpose of the following code. It is likely that it is some development code related to the use of BERT. Flagged as legacy.

# validation_type <- "spatial"
# validation_type <- "temporal"
validation_type <- "location"
validationHumanReading <- read.csv(paste0("F:/hguillon/research/exploitation/R/latin_america/data/validation_df_", validation_type, ".csv"))
validationHumanReading <- validationHumanReading[validationHumanReading$title != "", ]

titleInd_file <- "F:/hguillon/research/exploitation/R/latin_america/data/titleInd.Rds"
titleInd <- readRDS(titleInd_file)

englishCorpus_file <- "F:/hguillon/research/exploitation/R/latin_america/data/english_corpus.Rds"
englishCorpus <- readRDS(englishCorpus_file)

in_corpus_file <- "in_corpus.Rds"
in_corpus <- readRDS(in_corpus_file)

EndNoteIdcorpus <- unname(sapply(in_corpus$pdfs, substr, start = 1, stop = 10))
EndNoteIdLDA <- unname(sapply(englishCorpus$fnames, substr, start = 1, stop = 10))

table(EndNoteIdLDA %in% EndNoteIdcorpus)

EndNoteIdLDA <- EndNoteIdLDA[which(EndNoteIdLDA %in% EndNoteIdcorpus)]
englishCorpus <- englishCorpus[which(EndNoteIdLDA %in% EndNoteIdcorpus), ]
englishCorpus$abstract <- as.character(in_corpus$abstract[match(EndNoteIdLDA, EndNoteIdcorpus)])

# check that all the papers are found and address issues
table(is.na(titleInd))

# identify the subset of paper with validation data
validationHumanReading <- validationHumanReading[!is.na(titleInd), ]
titleInd <- na.omit(unlist(titleInd))

validationHumanReading <- validationHumanReading[!duplicated(titleInd), ]
titleInd <- unique(titleInd)

validationCorpus <- englishCorpus[titleInd, ]

validationCorpus <- validationCorpus[which(validationHumanReading$Country.1 != "0"), ]
validationHumanReading <- validationHumanReading[which(validationHumanReading$Country.1 != "0"), ]


prec <- sapply(unique(validationHumanReading$Country.1), function(country){
	predicted <- as.factor(grepl(country, validationCorpus$abstract))
	actual <- as.factor(grepl(country, validationHumanReading$Country.1))
	cm <- caret::confusionMatrix(predicted, actual, mode = "prec_recall", positive = "TRUE")
	prec <- cm$byClass["Precision"]
	return(prec)
})

recall <- sapply(unique(validationHumanReading$Country.1), function(country){
	predicted <- as.factor(grepl(country, validationCorpus$abstract))
	actual <- as.factor(grepl(country, validationHumanReading$Country.1))
	cm <- caret::confusionMatrix(predicted, actual, mode = "prec_recall", positive = "TRUE")
	prec <- cm$byClass["Recall"]
	return(prec)
})

df <- data.frame(country = unique(validationHumanReading$Country.1), precision = prec, recall = recall)
df <- df[order(df$country), ]

library(ggplot2)
library(reshape2)
library(cowplot)
library(forcats)

p1 <- ggplot(melt(df)) + 
	geom_point(aes(x=value, y= country)) +
	geom_segment(aes(x = 0, xend = value,  y = country, yend = country)) + 
	theme_minimal() + 
	facet_grid(. ~ variable)

p2 <- ggplot(melt(df)) + 
	geom_density(aes(x = value, color = variable, fill = variable), alpha = 0.5, bw = 0.05) +
	theme_minimal()

# PPV / Precision: What proportion of positive identifications was actually correct?
# Recall / Sensitivity: What proportion of actual positives was identified correctly?