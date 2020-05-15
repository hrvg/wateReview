#' Parse htlm page from Scopus to retrieve the abstract of a document
#' @param urlScopus URL of a document
#' @return Text of the abstract of the document
get.scopusAbstract <- function(urlScopus){
	webpageScopus <- read_html(as.character(urlScopus))
	scopusAbstract <- html_text(html_nodes(webpageScopus, "p")[8])
	return(scopusAbstract)
}

#' Use Elsevier API to retrieve the abstract of a document
#' @param DOI DOI (e.g. from Web of Science database)
#' @return Text of the abstract of the document
get.wosAbstract <- function(DOI){
	res <- abstract_retrieval(DOI, identifier = "doi", verbose = FALSE)
	wosAbstract <- res$content$`abstracts-retrieval-response`$coredata$`dc:description`
	return(wosAbstract)
}

#' Use Elsevier API to retrieve the author keywords of a document
#' @param DOI DOI (e.g. from Web of Science database) or the complete result from a previous request
#' @param from_Fullresult logical, if TRUE extracts the abstract from the result of a previous request
#' @return List of author keywords of the corresponding document
get.wosAuthKeywords <- function(DOI, from_FullResult = FALSE){
  if (from_FullResult == FALSE){
    res <- abstract_retrieval(DOI, identifier = "doi", verbose = FALSE)
    wosAuthKeywords <- unlist(res$content$`abstracts-retrieval-response`$authkeywords)
    wosAuthKeywords <- unname(wosAuthKeywords[c(FALSE, TRUE)])
  } else {
    res <- DOI
    if (!is.null(res[[1]]$content)){
      gunzipped <- memDecompress(res[[1]]$content, type = "none", asChar = TRUE)
      if (grepl("abstracts-retrieval-response", gunzipped)){
        content <- jsonlite::fromJSON(gunzipped)
        wosAuthKeywords <- unlist(content$`abstracts-retrieval-response`$authkeywords)
        wosAuthKeywords <- wosAuthKeywords[!grepl("@_fa", names(wosAuthKeywords))]
      } else {
        wosAuthKeywords <- NULL
      } 
    } else {
      wosAuthKeywords <- NA
    }
  }
  return(wosAuthKeywords)
}

#' Use DOI to extract metadata
#' @param DOI a DOI
#' @return result
get.wosFullResult <- function(DOI){
  res <- abstract_retrieval(DOI, identifier = "doi", verbose = FALSE)
  return(res)
}

#' Extract metadata from Scopus or Web of Science identifiers
#' @param documentID list of URLs for Scopus database or DOIs for Web of Science database
#' @param fun function, either get.scopusAbstract, get.wosAbstract or get.wosFullResult
#' @param newpass boolean, default to FALSE, if TRUE, ind_null and metadata are expected to be set
#' @param ind_null list, flags for failure to retrieve
#' @param metadata list, list of already retrieved metadata
#' @return a list of metadata: only abstracts or complete metadata
get.allMetadata <- function(documentID, fun = get.scopusAbstract, newpass = FALSE, metadata = NULL, ind_null = NULL){
    if (newpass){
      indices <- unname(which(ind_null == TRUE))
      metadata <- unname(metadata)
      n <- length(documentID)
    } else {
      indices <- seq_along(documentID)
      n <- length(indices)
      metadata <- list()
     }
    for (i in indices) {
      if (!(as.character(documentID)[i] %in% names(metadata))) {
        cat(paste0(round(i / n * 100), "% completed", " | Doing ", as.character(documentID)[i], " ..."))
        ok <- FALSE
        counter <- 0
        while (ok == FALSE & counter <= 5) {
          counter <- counter + 1
          out <- tryCatch({                  
            fun(as.character(documentID)[i])
          },
          error = function(e) {
            Sys.sleep(2)
            e
          }
          )
          if ("error" %in% class(out)) {
            cat(".")
          } else {
            ok <- TRUE
            cat(" Done.")
          }
        }
        cat("\n")
       metadata[[i]] <- ifelse(identical(fun, get.scopusAbstract), 
            out, 
            ifelse(is.null(out), NA, out)
            )
        names(metadata)[i] <- as.character(documentID)[i]
      }
    } 
    return(metadata)
}

#' Add retrieved abstract to corpus
#' @param in_corpus corpus database with ArticleURL column
#' @param scopusAbstracts abstracts extracted from Scopus database
#' @param wosAbstracts abstracts extracted from Web of Science database
#' @return corpus corpus database with a new column abstract
add.abstractsToCorpus <- function(in_corpus, scopusAbstracts, wosAbstracts){
  corpus <- in_corpus
  ind_scopus <- which(grepl("scopus", corpus$ArticleURL) == TRUE)
  ind_wos <- which(grepl("scopus", corpus$ArticleURL) == FALSE)
  corpus$abstract <- rep(NA, nrow(corpus))
  corpus$abstract[ind_scopus] <- scopusAbstracts
  corpus$abstract[ind_wos] <- wosAbstracts
  return(corpus)    
}

#' Extract countries names to be searched for in the author keywords
#' @return List of country names
get.relevantCountries <- function(){
  file <- '../latin_america_SQ_internship/data/countries_database.csv'
  countries_database <- read.csv(file, header = TRUE)
  relevant_countries <- as.character(countries_database$COUNTRY)
  relevant_countries <- c(relevant_countries, "Gulf of Mexico")
  return(relevant_countries)  
}

#' Extracts all author keywords from the metadata results
#' @param wosFullResult list, list of metadata
#' @return list of keywords
get.allAuthKeywords <- function(wosFullResult){
  lapply(wosFullResult, function(res) get.wosAuthKeywords(res, from_FullResult = TRUE))
}

#' Quality analysis on the author keywords retrieved
#' @param wosAuthKeywords list, list of author keywords
#' @param relevant_countries list, list of country names
#' @return list of failed entries
QA.AuthKeywords <- function(wosAuthKeywords, relevant_countries){
    ind_nullAuthKeywords <- sapply(wosAuthKeywords, is.null)
    ind_naAuthKeywords <- sapply(wosAuthKeywords, function(el) any(is.na(el)))
    ind_validAuthKeywords <- apply(cbind(ind_nullAuthKeywords, ind_naAuthKeywords), 1, function(row) all(row == FALSE))
    validAuthKeywords <- wosAuthKeywords[ind_validAuthKeywords]
    grepped_AuthKeywords <- sapply(relevant_countries, function(country){
      length(validAuthKeywords[grep(country, validAuthKeywords)])
    })
    print(grepped_AuthKeywords)
    return(ind_nullAuthKeywords)
}

#' Transform the author keywords into a multilabel dataset
#' @param wosAuthKeywords author keywords
#' @param relevant_countries labels to search for
#' @result a data.frame
get.boolean_AuthKeywords <- function(wosAuthKeywords, relevant_countries){
  boolean_AuthKeywords <- lapply(relevant_countries, function(country) grepl(country, wosAuthKeywords))
  boolean_AuthKeywords <- do.call(cbind, boolean_AuthKeywords)
  boolean_AuthKeywords <- data.frame(boolean_AuthKeywords)
  colnames(boolean_AuthKeywords) <- relevant_countries
  isGulfMexico <- boolean_AuthKeywords$`Gulf of Mexico`
  boolean_AuthKeywords$`Gulf of Mexico` <- NULL
  boolean_AuthKeywords$Mexico[isGulfMexico] <- FALSE
  return(boolean_AuthKeywords)
}

#' Identifies if a document has a tag
#' @param boolean_AuthKeywords data.frame of logical values indicating if a country has been found in the author keywords
#' @return list of logical
get.ind_hasCountryTag <- function(boolean_AuthKeywords){
  ind_hasCountryTag <- apply(boolean_AuthKeywords, 1, function(row) any(row == TRUE))
  return(ind_hasCountryTag)
}

#' Tokenize labels, here a list of relevant countries
#' @param webscrapped_trainingLabels data.frame of multiple labels
#' @return Tokenized labels
make.country_tokens <- function(webscrapped_trainingLabels){
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
  return(dic)
}

#' get document ID from EndNote query corpus database
#' @param in_corpus corpus database
#' @return list of document IDs
get.EndNoteIdcorpus <- function(in_corpus){
  EndNoteIdcorpus <- unname(sapply(in_corpus$pdfs, substr, start = 1, stop = 10))
  return(EndNoteIdcorpus)
}

#' get document ID from LDA corpus database
#' @param englishCorpus LDA corpus database
#' @return list of document IDs
get.EndNoteIdLDA <- function(englishCorpus){
  EndNoteIdLDA <- unname(sapply(englishCorpus$fnames, substr, start = 1, stop = 10))
  return(EndNoteIdLDA)
}

#' Performs a quick QA on both sources for document ID
#' @param EndNoteIdcorpus
#' @param EndNoteIdLDA
#' @return none
QA.EndNoteIdCorpusLDA <- function(EndNoteIdLDA, EndNoteIdcorpus){
  print("LDA in Corpus")
  print(table(EndNoteIdLDA %in% EndNoteIdcorpus))
  print("Corpus in LDA")
  print(table(EndNoteIdcorpus %in% EndNoteIdLDA))
}

#' Align databases based on shared ID
#' @param EndNoteIdLDA 
#' @param EndNoteIdcorpus
#' @return EndNoteIdLDA with matching records in both databases
align.dataWithEndNoteIdLDA <- function(data, EndNoteIdLDA, EndNoteIdcorpus){
     if(is.null(dim(data))){
    data <- data[which(EndNoteIdLDA %in% EndNoteIdcorpus)]
  } else {
    data <- data[which(EndNoteIdLDA %in% EndNoteIdcorpus), ]
  }
  return(data)
}

#' Align databases based on shared ID
#' @param EndNoteIdcorpus
#' @param EndNoteIdLDA 
#' @return EndNoteIdcorpus with matching records in both databases
align.dataWithEndNoteIdcorpus <- function(data, EndNoteIdcorpus, EndNoteIdLDA){
    if(is.null(dim(data))){
    data <- data[which(EndNoteIdcorpus %in% EndNoteIdLDA)]
  } else {
    data <- data[which(EndNoteIdcorpus %in% EndNoteIdLDA), ]
  }
  return(data)
}

#' Align englishCorpus with matching records in both databases and assign webscrapped abstracts
#' @param englishCorpus corpus of document
#' @param EndNoteIdcorpus
#' @param EndNoteIdLDA 
#' @return englishCorpus with matching records in both databases and webscrapped abstracts
align.englishCorpus <- function(englishCorpus, EndNoteIdLDA, EndNoteIdcorpus, in_corpus){
  englishCorpus <- englishCorpus[which(EndNoteIdLDA %in% EndNoteIdcorpus), ]
  englishCorpus$abstract <- as.character(in_corpus$abstract[which(EndNoteIdcorpus %in% EndNoteIdLDA)][match(EndNoteIdLDA, EndNoteIdcorpus)])
  return(englishCorpus)
}

#' Order data to LDA order
#' @param data data: corpus of documents, indices of tagged document, ...
#' @param EndNoteIdcorpus
#' @param EndNoteIdLDA 
#' @return data with matching records in both databases and webscrapped abstracts
order.data <- function(data, EndNoteIdLDA, EndNoteIdcorpus){
  if(is.null(dim(data))){
    data <- data[match(EndNoteIdLDA, EndNoteIdcorpus)]
  } else {
    data <- data[match(EndNoteIdLDA, EndNoteIdcorpus), ]
  }
  return(data)
}

#' Create a data.frame of multilabels using the webscrapped author keywords
#' @param boolean_AuthKeywords data.frame of multilabels webscrapped author keywords
#' @param ind_hasCountryTag list of boolean indicating if an entry has at least one label
#' @param englishCorpus databse of corpus of document with abstracts
#' @param englishCorpus_file file path to the complete corpus
#' @return list with 3 elements: country_tokens: tokenized country labels, webscrapped_validationDTM: a document term matrix derived from the tokenized country labels, webscrapped_trainingLabels: webscrapped multilabels
make.webscrapped_trainingData <- function(boolean_AuthKeywords, ind_hasCountryTag, englishCorpus, englishCorpus_file){
  # subset database for which entries have a label
  englishCorpus_hasTag <- englishCorpus[ind_hasCountryTag, ]
  boolean_AuthKeywords_hasTag <- boolean_AuthKeywords[ind_hasCountryTag, ]

  # make webscrapped_trainingLabels
  webscrapped_trainingLabels <- boolean_AuthKeywords_hasTag %>% dplyr::mutate(Irrelevant = FALSE)

  # removing NULL abstract
  # ind_nonNullnonNA <- which(!(englishCorpus_hasTag$abstract == "NULL" | englishCorpus_hasTag$abstract == "NA"))
  # englishCorpus_hasTag <- englishCorpus_hasTag[ind_nonNullnonNA, ]
  # webscrapped_trainingLabels <- webscrapped_trainingLabels[ind_nonNullnonNA, ]

  # removing human read files
  # import::here(.from = "./R/utils/lib_shared.R", get_titleInd)
  # titleInd <- get_titleInd()
  englishCorpus_complete <- readRDS(englishCorpus_file)
  # ind_HumanRead_hasTag <- which(englishCorpus_hasTag$fnames %in% englishCorpus[titleInd, ]$fnames)
  # englishCorpus_hasTag <- englishCorpus_hasTag[-ind_HumanRead_hasTag, ]
  # webscrapped_trainingLabels <- webscrapped_trainingLabels[-ind_HumanRead_hasTag, ]

  # read document term matrix file
  dtm_file <- "F:/hguillon/research/exploitation/R/latin_america/data/obj_dtm_from_dfm_geo.Rds"
  obj_dtm <- readRDS(dtm_file)
  webscrapped_validationDTM <- obj_dtm[which(englishCorpus_complete$fnames %in% englishCorpus_hasTag$fnames), ]
  webscrapped_validationDTM <- as.matrix(webscrapped_validationDTM)

  # get country tokens
  country_tokens <- make.country_tokens(webscrapped_trainingLabels)
  
  # adjust colnames
  colnames(webscrapped_validationDTM) <- paste0("Term", seq(ncol(webscrapped_validationDTM)))
  colnames(webscrapped_trainingLabels) <- gsub(" ", ".", colnames(webscrapped_trainingLabels))
  
  webscrapped_trainingData <- list(
    country_tokens = country_tokens,
    webscrapped_validationDTM = webscrapped_validationDTM,
    webscrapped_trainingLabels = webscrapped_trainingLabels
    )
  return(webscrapped_trainingData)
}