#####################
##### LIBRARIES #####
#####################

library(pdftools)

#################
##### UTILS #####
#################

import::here(.from = "./R/utils/envpath.R", 
	get_rootdir)

########################
##### INITIALIZING #####
########################

root.dir <- get_rootdir()

################
##### MAIN #####
################

get_mail <- function(pdf){
	text <- pdf_text(pdf)
	text2 <- strsplit(text, "\n")
	i1 <- grep("@", text2)
	if (length(i1) >= 1) {
		mails <- lapply(i1, function(ii){
			i2 <- grep("@", text2[[ii]])
			text3 <- lapply(i2, function(i) unlist(strsplit(text2[[ii]][[i]], " ")))
			mail <- lapply(text3, function(txt) txt[grep("@", txt)])
			mail <- gsub("\r", "", mail)
			mail <- gsub("\n", "", mail)
			return(mail)
		})
		return(unlist(mails))
	} else {
		warning("More than one email address! (or less than one email address)")
		return(length(i1))
	}
}

lang <- "english"
pdf.dir <- paste0("data/latin_america/corpus_pdf/", lang, "/", lang, ".Data")

lf <- list.files(file.path(root.dir, pdf.dir), recursive = TRUE, pattern = ".pdf", full.names = TRUE)
mails <- unlist(lapply(lf, get_mail))
write.table(mails, file.path(root.dir, pdf.dir, paste0("mails_", lang, ".csv")), row.names = FALSE, col.names = FALSE)