get_sourceID <- function(res){
	if (!is.null(res[[1]]$content)){
		gunzipped <- memDecompress(res[[1]]$content, type = "none", asChar = TRUE)
		if (grepl("abstracts-retrieval-response", gunzipped)){
	    	content <- jsonlite::fromJSON(gunzipped)
			# source <- content$`abstracts-retrieval-response`$item$bibrecord$head$`citation-title`
			# citing <- na.omit(unlist(content$`abstracts-retrieval-response`$item$bibrecord$tail$bibliography$reference$`ref-info`$`ref-title`))
			itemidlist <- content$`abstracts-retrieval-response`$item$bibrecord$`item-info`$itemidlist$itemid
			return(itemidlist$`$`[itemidlist["@idtype"] == "SGR"])
		}
	}
	return(NA)
}


get_citing <- function(res){
	if (!is.null(res[[1]]$content)){
		gunzipped <- memDecompress(res[[1]]$content, type = "none", asChar = TRUE)
		if (grepl("abstracts-retrieval-response", gunzipped)){
	    	content <- jsonlite::fromJSON(gunzipped)
	    	citing <- content$`abstracts-retrieval-response`$item$bibrecord$`item-info`$itemidlist$itemid
	    	citing <- citing$`$`[citing["@idtype"] == "SGR"]
	    	cited <- content$`abstracts-retrieval-response`$item$bibrecord$tail$bibliography$reference$`ref-info`$`refd-itemidlist`$itemid
	    	cited <- cited$`$`[cited["@idtype"] == "SGR"]
	    	cited <- cited[which(cited %in% source_ids)]
	    	if (length(cited) >= 1){
	    		citing <- rep(citing, length(cited))
	    		return(data.frame(citing =citing, cited = cited))
	    	} else {
	    		return(NA)
	    	}
		}
	}
	return(NA)
}