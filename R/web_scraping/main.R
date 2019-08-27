#################
### libraries ###
#################

if(!require("rvest")) install.packages("rvest")
library("rvest")

if(!require("rscopus")) install.packages("rscopus")
library("rscopus")
rscopus::set_api_key("b8b0698af42a2a93b41dc902260bde9d")
rscopus::set_api_key("eef27a968a88e689a8c52f0ae35676f6")
rscopus::set_api_key("38493e05eff904cef27135964f34d2b2")
#################
##### utils #####
#################

import::here(.from = "./R/utils/envpath.R", 
	get_rootdir)

import::here(.from = "./R/utils/lib_query.R",
	get_meta_df,
	get_language_dfs,
	make_pretty_str
	)

#################
### functions ###
#################

get.scopusAbstract <- function(urlScopus){
	webpageScopus <- read_html(as.character(urlScopus))
	scopusAbstract <- html_text(html_nodes(webpageScopus, "p")[8])
	return(scopusAbstract)
}

get.wosAbstract <- function(DOI){
	res <- abstract_retrieval(DOI, identifier = "doi", verbose = FALSE)
	wosAbstract <- res$content$`abstracts-retrieval-response`$coredata$`dc:description`
	return(wosAbstract)
}

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

get.wosFullResult <- function(DOI){
  res <- abstract_retrieval(DOI, identifier = "doi", verbose = FALSE)
  return(res)
}

##############
#### init ####
##############

root.dir <- get_rootdir()
languages <- c("english", "portuguese", "spanish")
out.dir <- "exploitation/out/run79"

##############
#### main ####
##############

load(file = file.path(root.dir, out.dir, paste0("language_dfs_updated_2", ".rda")))

in_corpus <- language_dfs$english[which(language_dfs$english$collected == "in corpus"), ]

in_scopus <- in_corpus$ArticleURL[grepl("scopus", in_corpus$ArticleURL)]
in_wos <- in_corpus[!grepl("scopus", in_corpus$ArticleURL), ]
in_wos <- in_wos$DOI

scopusAbstracts<- list()
for (i in seq_along(as.character(in_scopus))) {
  if (!(as.character(in_scopus)[i] %in% names(scopusAbstracts))) {
    cat(paste("Doing", as.character(in_scopus)[i], "..."))
    ok <- FALSE
    counter <- 0
    while (ok == FALSE & counter <= 5) {
      counter <- counter + 1
      out <- tryCatch({                  
        get.scopusAbstract(as.character(in_scopus)[i])
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
   scopusAbstracts[[i]] <- out
    names(scopusAbstracts)[i] <- as.character(in_scopus)[i]
  }
} 

wosAbstracts <- list()
for (i in seq_along(as.character(in_wos))) {
  if (!(as.character(in_wos)[i] %in% names(wosAbstracts))) {
    cat(paste("Doing", as.character(in_wos)[i], "..."))
    ok <- FALSE
    counter <- 0
    while (ok == FALSE & counter <= 5) {
      counter <- counter + 1
      out <- tryCatch({                  
        get.wosAbstract(as.character(in_wos)[i])
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
   wosAbstracts[[i]] <- ifelse(is.null(out), NA, out)
   names(wosAbstracts)[i] <- as.character(in_wos)[i]
  }
}

ind_scopus <- which(grepl("scopus", in_corpus$ArticleURL) == TRUE)
ind_wos <- which(grepl("scopus", in_corpus$ArticleURL) == FALSE)
in_corpus$abstract <- rep(NA, nrow(in_corpus))
in_corpus$abstract[ind_scopus] <- scopusAbstracts
in_corpus$abstract[ind_wos] <- wosAbstracts

##############
### saving ###
##############

saveRDS(in_corpus, "in_corpus.Rds")

###################
### full_result ###
###################

# init
wosFullResult = list()
indices <- seq_along(as.character(in_corpus$DOI))
n <- length(indices)
for (i in indices) {
  if (!(as.character(in_corpus$DOI)[i] %in% names(wosFullResult))) {
    cat(paste0(round(i / n * 100), "% completed", " | Doing ", as.character(in_corpus$DOI)[i], " ..."))
    ok <- FALSE
    counter <- 0
    while (ok == FALSE & counter <= 5) {
      counter <- counter + 1
      out <- tryCatch({                  
        get.wosFullResult(as.character(in_corpus$DOI)[i])
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
    cat('\n')
   wosFullResult[[i]] <- ifelse(is.null(out), NA, out)
   names(wosFullResult)[i] <- as.character(in_corpus$DOI)[i]
  }
}

saveRDS(wosFullResult, "wosFullResult.Rds")


################
### keywords ###
################

wosAuthKeywords <- lapply(wosFullResult, function(res) get.wosAuthKeywords(res, from_FullResult = TRUE))

ind_nullAuthKeywords <- sapply(wosAuthKeywords, is.null)
ind_naAuthKeywords <- sapply(wosAuthKeywords, function(el) any(is.na(el)))
ind_validAuthKeywords <- apply(cbind(ind_nullAuthKeywords, ind_naAuthKeywords), 1, function(row) all(row == FALSE))
validAuthKeywords <- wosAuthKeywords[ind_validAuthKeywords]

relevant_countries <- c("Argentina", "Belize", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", "Ecuador", "El Salvador", "Guatemala", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "Uruguay", "Venezuela", "Gulf of Mexico")


grepped_AuthKeywords <- sapply(relevant_countries, function(country){
  length(validAuthKeywords[grep(country, validAuthKeywords)])
})

##################################
### transform results to label ###
##################################

boolean_AuthKeywords <- lapply(relevant_countries, function(country) grepl(country, wosAuthKeywords))
boolean_AuthKeywords <- do.call(cbind, boolean_AuthKeywords)
boolean_AuthKeywords <- data.frame(boolean_AuthKeywords)
colnames(boolean_AuthKeywords) <- relevant_countries
isGulfMexico <- boolean_AuthKeywords$`Gulf of Mexico`
boolean_AuthKeywords$`Gulf of Mexico` <- NULL
boolean_AuthKeywords$Mexico[isGulfMexico] <- FALSE
ind_hasCountryTag <- apply(boolean_AuthKeywords, 1, function(row) any(row == TRUE))

saveRDS(ind_hasCountryTag, "ind_hasCountryTag.Rds")
saveRDS(boolean_AuthKeywords, "boolean_AuthKeywords.Rds")

###############################
### retrying failed entries ###
###############################

### below code is intended for retrying some failing entries

indices <- unname(which(ind_nullAuthKeywords == TRUE))
wosFullResult <- unname(wosFullResult)
n <- length(seq_along(as.character(in_corpus$DOI)))
for (i in indices) {
  if (!(as.character(in_corpus$DOI)[i] %in% names(wosFullResult))) {
    cat(paste0(round(i / n * 100), "% completed", " | Doing ", as.character(in_corpus$DOI)[i], " ..."))
    ok <- FALSE
    counter <- 0
    while (ok == FALSE & counter <= 5) {
      counter <- counter + 1
      out <- tryCatch({                  
        get.wosFullResult(as.character(in_corpus$DOI)[i])
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
    cat('\n')
   wosFullResult[[i]] <- ifelse(is.null(out), NA, out)
   names(wosFullResult)[i] <- as.character(in_corpus$DOI)[i]
  }
}

