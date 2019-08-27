library("quanteda")

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

sorted_dfm <- dfm_sort(obj_dfm, decreasing = TRUE, margin = c("features"))
feats <- featnames(sorted_dfm)

dic <- colnames(webscrapped_trainingLabels)[-1]
dic <- gsub("Costa.Rica", "Costa", dic)
dic <- gsub("El.Salvador", "Salvador", dic)
dic <- gsub("Honduras", "Hondura", dic)
dic <- gsub("Belize", "Beliz", dic)


obj_dtm <- sorted_dfm[, sapply(tolower(dic), function(c) which(feats == c))]
dtm_file <- "F:/hguillon/research/exploitation/R/latin_america/data/obj_dtm_from_dfm_country.Rds"
saveRDS(obj_dtm, dtm_file)

geo_dic <- c("america", "amazon", "andes", "altiplano", "patagonia", "guiana", "orinoco", "savana", "swamp", "savan", "llano", "grassland", "parana", "plata", "pampa", "atacama", "iguazu", "highland", "pacif", "atlant", "gulf", "cordillera", "isthmus", "archipelago", "bravo", "sierra", "karst", "cenot", "titicaca", "cotopaxi")

full_dic <- c(dic, geo_dic)

sort(sapply(tolower(full_dic), function(c) which(feats == c)))

obj_dtm <- sorted_dfm[, c(86:(5000+86-1))]
dtm_file <- "F:/hguillon/research/exploitation/R/latin_america/data/obj_dtm_from_dfm.Rds"
saveRDS(obj_dtm, dtm_file)