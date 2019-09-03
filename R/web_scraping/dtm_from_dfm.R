library("quanteda")

dfm_file <- "F:/hguillon/research/exploitation/R/latin_america/data/obj_dfm.Rds"
if (!file.exists(dfm_file)){
	englishCorpus_file <- "F:/hguillon/research/exploitation/R/latin_america/data/english_corpus.Rds"
	englishCorpus <- readRDS(englishCorpus_file)
	obj_dfm <- dfm(englishCorpus$raw, 
		tolower = TRUE,
		stem = TRUE,
		remove = c(stopwords("en"), stopwords("es"), stopwords("pt")),
		remove_punct = TRUE,
		remove_symbols = TRUE,
		remove_numbers = TRUE,
		remove_separators = TRUE,
		remove_hyphens = TRUE,
		remove_url = TRUE)

	obj_dfm <- dfm_remove(obj_dfm, min_nchar = 4)
	saveRDS(obj_dfm, dfm_file)
} else {
	obj_dfm <- readRDS(dfm_file)
}

sorted_dfm <- dfm_sort(obj_dfm, decreasing = TRUE, margin = c("features"))
feats <- featnames(sorted_dfm)

country_tokens <- readRDS("./data/country_tokens.Rds")

# dic <- gsub("Costa.Rica", "Costa", dic)
# dic <- gsub("El.Salvador", "Salvador", dic)
# dic <- gsub("Honduras", "Hondura", dic)
# dic <- gsub("Belize", "Beliz", dic)
# dic <- gsub("Trinidad and Tobago", "Trinidad", dic)
# dic <- gsub("St. Lucia", "Lucia", dic)
# dic <- gsub("St. Litts", "Lucia", dic)

country_max <- max(sapply(tolower(country_tokens), function(c) which(feats == c)))

sorted_dfm <- sorted_dfm[, 1:country_max]
feats <- featnames(sorted_dfm)

obj_dtm <- sorted_dfm[, na.omit(match(country_tokens, feats))]
dtm_file <- "F:/hguillon/research/exploitation/R/latin_america/data/obj_dtm_from_dfm_country.Rds"
saveRDS(obj_dtm, dtm_file)

geo_dic <- c("america", "amazon", "andes", "altiplano", "patagonia", "guiana", "orinoco", "savana", "swamp", "savan", "llano", "grassland", "parana", "plata", "pampa", "atacama", "iguazu", "highland", "pacif", "atlant", "gulf", "cordillera", "isthmus", "archipelago", "bravo", "sierra", "karst", "cenot", "titicaca", "cotopaxi")

full_dic <- c(country_tokens, geo_dic)

obj_dtm <- sorted_dfm[, c(86:(5000+86-1))]
dtm_file <- "F:/hguillon/research/exploitation/R/latin_america/data/obj_dtm_from_dfm.Rds"
saveRDS(obj_dtm, dtm_file)

geographical_tokens <- readRDS("./data/geographical_tokens.Rds")

full_tokens <- unique(c(country_tokens, geographical_tokens))
obj_dtm <- sorted_dfm[, na.omit(match(full_tokens, feats))]
dtm_file <- "F:/hguillon/research/exploitation/R/latin_america/data/obj_dtm_from_dfm_geo.Rds"
saveRDS(obj_dtm, dtm_file)