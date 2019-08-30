dic <- tokens(colnames(webscrapped_trainingLabels)[-1], 
	remove_punct = TRUE,
	remove_symbols = TRUE,
	remove_numbers = TRUE,
	remove_separators = TRUE,
	remove_hyphens = TRUE,
	remove_url = TRUE)
dic <- tokens_wordstem(dic)
dic <- tokens_select(dic, min_nchar = 4)
dic <- unname(unlist(dic))
dic <- tolower(dic)