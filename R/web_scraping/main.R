#####################
##### LIBRARIES #####
#####################

library(ggplot2)
library(forcats)
library(plyr)
library(dplyr)
library(cowplot)
library(gridExtra)
library(multcompView)

#################
##### UTILS #####
#################

import::here(.from = "./R/utils/envpath.R", 
	get_rootdir)

import::here(.from = "./R/utils/lib_query.R",
	get_meta_df,
	get_language_dfs,
	make_pretty_str
	)

##############
#### INIT ####
##############

root.dir <- get_rootdir()
languages <- c("english", "portuguese", "spanish")
out.dir <- "exploitation/out/run79"

##############
#### MAIN ####
##############

load(file = file.path(root.dir, out.dir, paste0("language_dfs_updated_2", ".rda")))


in_corpus <- language_dfs$english[which(language_dfs$english$collected == "in corpus"), ]

in_scopus <- in_corpus$ArticleURL[grepl("scopus", in_corpus$ArticleURL)]
in_wos <- in_corpus[!grepl("scopus", in_corpus$ArticleURL), ]

#################
### libraries ###
#################

if(!require("rvest")) install.packages("rvest")
library("rvest")

##################
### parameters ###
##################

idFileName <- "something.csv"

#################
### functions ###
#################

get.scopusAbstract <- function(urlScopus){
	webpageScopus <- read_html(as.character(urlScopus))
	scopusAbstract <- html_text(html_nodes(webpageScopus, "p")[8])
	return(scopusAbstract)
}


rscopus::set_api_key("b8b0698af42a2a93b41dc902260bde9d")


#previewAbstract1

# 10.1002/hyp.10995

# urlScopus <- "https://www.scopus.com/results/results.uri?numberOfFields=0&src=s&clickedLink=&edit=&editSaveSearch=&origin=searchbasic&authorTab=&affiliationTab=&advancedTab=&scint=1&menu=search&tablin=&searchterm1=+10.1002%2Fhyp.10995&field1=DOI&dateType=Publication_Date_Type&yearFrom=Before+1960&yearTo=Present&loadDate=7&documenttype=All&accessTypes=All&resetFormLink=&st1=+10.1002%2Fhyp.10995&st2=&sot=b&sdt=b&sl=23&s=DOI%28+10.1002%2Fhyp.10995%29&sid=96c83fd35d964bf36a4c276d57a5e93b&searchId=96c83fd35d964bf36a4c276d57a5e93b&txGid=621df8975ccb5e827795e1ecc578e2a9&sort=plf-f&originationType=b&rr="
# webpageScopus <- read_html(as.character(urlScopus))

############
### main ###
############

geneNames<- list()
for (i in seq_along(as.character(data_ens$id))) {
  if (!(as.character(data_ens$id)[i] %in% names(geneNames))) {
    cat(paste("Doing", as.character(data_ens$id)[i], "..."))
    ok <- FALSE
    counter <- 0
    while (ok == FALSE & counter <= 5) {
      counter <- counter + 1
      out <- tryCatch({                  
        get.geneNameNCBI(as.character(data_ens$id)[i])
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
   geneNames[[i]] <- out
    names(geneNames)[i] <- as.character(data_ens$id)[i]
  }
} 