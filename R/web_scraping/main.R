### libraries ###

if(!require("rvest")) install.packages("rvest")
library("rvest")

if(!require("rscopus")) install.packages("rscopus")
library("rscopus")

### utils ###
import::here(.from = "./R/utils/envpath.R", 
	get_rootdir)

import::here(.from = "./R/utils/lib_query.R",
	get_meta_df,
	get_language_dfs,
	make_pretty_str
	)

import::here(.from = "./R/utils/lib_webscrapping.R",
  get.scopusAbstract,
  get.wosAbstract,
  get.wosAuthKeywords,
  get.wosFullResult,
  get.allMetadata,
  add.abstractsToCorpus,
  get.relevantCountries,
  get.allAuthKeywords,
  QA.AuthKeywords,
  get.boolean_AuthKeywords,
  get.ind_hasCountryTag
)

### main ###

# init

root.dir <- get_rootdir()
languages <- c("english", "portuguese", "spanish")
out.dir <- "exploitation/out/run79"
rscopus::set_api_key("38493e05eff904cef27135964f34d2b2")

# data loading

load(file = file.path(root.dir, out.dir, paste0("language_dfs_updated_2", ".rda")))
in_corpus <- language_dfs$english[which(language_dfs$english$collected == "in corpus"), ]
scopusID <- in_corpus$ArticleURL[grepl("scopus", in_corpus$ArticleURL)]
wosID <- in_corpus[!grepl("scopus", in_corpus$ArticleURL), ]
wosID <- wosID$DOI

# abstract retrieval

scopusAbstracts <- get.allMetadata(scopusID, fun = get.scopusAbstract)
wosAbstracts <- get.allMetadata(wosID, fun = get.wosAbstract)
in_corpus <- add.abstractsToCorpus(in_corpus, scopusAbstracts, wosAbstracts)
saveRDS(in_corpus, "in_corpus.Rds")

# full_result

wosFullResult <- get.allMetadata(in_corpus$DOI, fun = get.wosFullResult)

# keywords

relevant_countries <- get.relevantCountries()
wosAuthKeywords <- get.allAuthKeywords(wosFullResult)
ind_nullAuthKeywords <- QA.AuthKeywords(wosAuthKeywords, relevant_countries)
saveRDS(wosAuthKeywords, "wosAuthKeywords.Rds")

# iterate (optional)

# at this point, it is possible to iterate and retry some of the failed entries
# wosFullResult <- get.allMetadata(in_corpus$DOI, fun = get.wosFullResult, newpass = TRUE, metadata = wosFullResult, ind_null = ind_nullAuthKeywords)

# transform results to label

boolean_AuthKeywords <- get.boolean_AuthKeywords(wosAuthKeywords, relevant_countries)
saveRDS(boolean_AuthKeywords, "boolean_AuthKeywords.Rds")

# identify entries with any label

ind_hasCountryTag <- get.ind_hasCountryTag(boolean_AuthKeywords)
saveRDS(ind_hasCountryTag, "ind_hasCountryTag.Rds")