library("quanteda")

# data loading

## raw corpus

corpusFile <- "../../../data/latin_america/water-management/english_corpus.rds"
englishCorpus <- readRDS(corpusFile)

## funding look-up table
fundingFile <- "data/funding_sources2.csv"
fundingLookup <- read.csv(fundingFile, header = FALSE)
colnames(fundingLookup) <- c("text", "label")
fundingLookup$text <- as.character(fundingLookup$text)
fundingLookup$label <- as.character(fundingLookup$label)
fundingLookup$label[fundingLookup$label == ""] <- NA
fundingLookup$label <- gsub("Non-governmental organization", "NGO", fundingLookup$label)
fundingLookup$label <- gsub("Intergovernmental organization ", "Intergovernmental organization", fundingLookup$label)
fundingLookup$label <- gsub("EPSRC", "Government", fundingLookup$label)
fundingLookup$label <- gsub(" Government", "Government", fundingLookup$label)
fundingLookup <- na.omit(fundingLookup)

# term counting

## costum dic
lbls <- unique(fundingLookup$label)
dic <- lapply(lbls, function(lbl) fundingLookup$text[fundingLookup$label == lbl])
names(dic) <- lbls
dic <- dictionary(dic)

## DFM
dfm <- dfm(englishCorpus$raw, dictionary = dic)
saveRDS(dfm, "funding_dfm.Rds")N