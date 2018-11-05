
get_endnote_xml <- function(language){
	pdf.dir <- paste0("data/latin_america/corpus_pdf/", language)	
	doc <- xmlParse(file.path(root.dir, pdf.dir, paste0(language, "_database.xml")))
	xmldf <- xmlToDataFrame(nodes = getNodeSet(doc, "//record"))
	# extract urls
	urls <- sapply(xmldf$urls, function(url){
		unlist(strsplit(url, "internal-pdf://"))[1]
	})
	# extract pdf path
	pdfs <- unname(sapply(xmldf$urls, function(url){
		unlist(strsplit(url, "internal-pdf://"))[2]
	}))
	xmldf$pdfs <- pdfs
	# extra formating 
	xmldf$dates <- as.numeric(sub("//", "", xmldf$dates))
	return(xmldf)
}

get_endnote_titles <- function(language){
	csv.dir <- paste0("data/latin_america/corpus_csv/", language, "/")
	csv.file <- paste0(language, '_title_all.txt')
	endnote_titles <- readLines(file.path(root.dir, csv.dir, csv.file))
	return(endnote_titles)
}

