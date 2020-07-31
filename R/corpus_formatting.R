#' Get the list of files in a given directory
#' @param pdf_dir character of file.path pointing to a directory containing the document pdf files
#' @return a named list with two elements: `names` with the short names of the pdf files and `full_names` with the complete path to the pdf files.
#' @export
get_pdf_files <- function(pdf_dir){
	pdf_files <- list.files(file.path(pdf_dir), pattern = ".pdf", recursive = TRUE, full.names = FALSE)
	pdf_files <- sapply(pdf_files, function(file) tools::file_path_sans_ext(tail(unlist(strsplit(file, "/")), 1)))
	pdf_files.full <- list.files(file.path(pdf_dir), pattern = ".pdf", recursive = TRUE, full.names = TRUE)
	return(list(names = pdf_files, full_names = pdf_files.full))
}

#' Get the indices of duplicated documents
#' @param names character, document names
#' @return a list of indices of non-duplicate documents
#' @export
get_non_duplicate_pdfs <- function(names){
	ind <- which(!duplicated(names))
	print(length(names))
	print(length(ind))
	return(ind)
}

#' Randomly select articles for human-reading
#' @param full.names file.paths to corpus document
#' @param non_duplicate_index index of non-duplicated documents
#' @param ratio ratio of total number of documents to select; defaults to 20%
#' @param .seed random seed for reproducible results; default to 89
#' @export
article_selection <- function(full.names, non_duplicate_index, ratio = .2, .seed = 89){
	set.seed(.seed)
	n <- floor(ratio * length(non_duplicate_index))
	select_ind <- sample(non_duplicate_index, n)
	return(select_ind)
}

#' Chunk the selected documents between a given number of human readers and copy them into a given directory
#' @param select_ind index of the selected documents
#' @param number_of_readers numeric, number of readers
#' @param out_dir directory to output the files
#' @export
assign_articles_to_readers <- function(select_ind, number_of_readers, out_dir){
	selected_full_names <- full_names[select_ind]
	selected_names <- names[select_ind]
	chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
	chunked_full_names <- chunk2(selected_full_names, number_of_readers)
	chunked_names <- chunk2(selected_names, number_of_readers)
	for (i in 1:number_of_readers){
		dir.create(file.path(out_dir, paste0("reader_", i)))
		id <- sample(seq(chunked_full_names[[i]]))
		lapply(seq_along(chunked_full_names[[i]]), function(j){
			file.copy(from = file.path(out_dir, chunked_full_names[[i]][j]), to = file.path(out_dir, paste0("reader_", i), paste0(id[j], "_", chunked_names[[i]][j])))
		})
	}
}

#' Fix the format of the document names from an existing database
#' @param lang character defining the language, one of: "english", "portuguese", "spanish"
#' @param full_names character, the list of full file.path to the document files
#' @param language_df a named list of data.frame, at least one name should correspond to `lang`. The resulting data.frame should have a `pdfs` column
#' @return a named list with two elements: `names` and `full_names`
#' @export 
fix_names <- function(lang, full_names, language_df = NULL){
	expected_lang <- c("english", "portuguese", "spanish")
	if(!lang %in% expected_lang) stop("Unsupported language.")
	if(!is.null(language_df)){
		full_names <- language_dfs[[lang]]$pdfs
	}

	full_names <- full_names[!is.na(full_names)]

	if (lang %in% c("spanish", "portuguese")){
		ind <- grep("manual_download", full_names)
		full_names[-ind] <- paste0(lang, '.Data/PDF/', full_names[-ind])
	} else {
		full_names <- paste0(lang, '.Data/PDF/', full_names)
	}

	names <- unlist(lapply(full_names, function(name){
		tail(strsplit(name, "/")[[1]], 1)
	}))

	if (lang == "english"){
		read_articles <- list.files(file.path(root_dir, out_dir, lang, "read_articles"), recursive = TRUE)
		read_articles <- unlist(lapply(read_articles, function(name){
			tail(strsplit(name, "/")[[1]], 1)
		}))
		read_articles <- unlist(lapply(read_articles, function(name){
			strsplit(name, "_")[[1]][-1]
		}))
		read_articles <- paste0(read_articles, ".pdf")
		ind <- which(names %in% read_articles)
		names <- names[-ind]
		full_names <- full_names[-ind]
	}
	return(list(names = names, full_names = full_names))
}