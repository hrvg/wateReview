dic <- colnames(webscrapped_trainingLabels)[-1]
dic <- gsub("Costa.Rica", "Costa", dic)
dic <- gsub("El.Salvador", "Salvador", dic)
vec <- VectorSource(englishCorpus_complete$raw)
obj_corpus <- Corpus(vec)
obj_dtm <- DocumentTermMatrix(obj_corpus, 
	control = list(
		weighting = weightTfIdf, 
		dictionary = tolower(dic)
		)
	)
dtm_file <- "F:/hguillon/research/exploitation/R/latin_america/data/obj_dtm_country.Rds"
saveRDS(obj_dtm_country, dtm_file)