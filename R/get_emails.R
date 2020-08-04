#' Extract email addresses from `pdf` documents
#' You probably want to execute this code on a linux server to avoid the issues with special character handling on Windows
#' @param pdf path to a `.pdf` files
#' @return emails addresses found in the `.pdf` document
#' @export
get_mail <- function(pdf){
	# print(pdf)
	if (file.size(pdf) > 0){
		# if (lang == "spanish"){
		# 	pdf <- gsub("i´", "í", pdf)
		# 	pdf <- gsub("e´", "é", pdf)
		# }
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
}