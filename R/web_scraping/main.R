#################
### libraries ###
#################

if(!require("rvest")) install.packages("rvest")
library("rvest")

if(!require("rscopus")) install.packages("rscopus")
library("rscopus")
rscopus::set_api_key("b8b0698af42a2a93b41dc902260bde9d")

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

get.wosAuthKeywords <- function(DOI){
  res <- abstract_retrieval(DOI, identifier = "doi", verbose = FALSE)
  wosAuthKeywords <- unlist(res$content$`abstracts-retrieval-response`$authkeywords)
  wosAuthKeywords <- unname(wosAuthKeywords[c(FALSE, TRUE)])
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

wosFullResult <- list()
n <- length(in_corpus$DOI)
for (i in seq_along(as.character(in_corpus$DOI))) {
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
