get_pdf_files <- function(root.dir, pdf.dir){
	pdf.files <- list.files(file.path(root.dir, pdf.dir), pattern = ".pdf", recursive = TRUE, full.names = FALSE)
	pdf.files <- sapply(pdf.files, function(file) tools::file_path_sans_ext(tail(unlist(strsplit(file, "/")), 1)))
	pdf.files.full <- list.files(file.path(root.dir, pdf.dir), pattern = ".pdf", recursive = TRUE, full.names = TRUE)
	return(list(names = pdf.files, full.names = pdf.files.full))
}

get_duplicate_pdfs <- function(names){
	ind <- which(!duplicated(names))
	print(length(names))
	print(length(ind))
	return(ind)
}

article_selection <- function(full.names, non_duplicate_index, ratio = .2){
	set.seed(89)
	n <- floor(ratio * length(full.names))
	select_ind <- sample(non_duplicate_index, n)
	return(select_ind)
}

assign_articles_to_readers <- function(select_ind, number_of_readers, out.dir){
	selected.full.names <- full.names[select_ind]
	selected.names <- names[select_ind]
	chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
	chunked.full.names <- chunk2(selected.full.names, number_of_readers)
	chunked.names <- chunk2(selected.names, number_of_readers)
	for (i in 1:number_of_readers){
		dir.create(file.path(root.dir, out.dir, paste0("reader_", i)))
		id <- sample(seq(chunked.full.names[[i]]))
		lapply(seq_along(chunked.full.names[[i]]), function(j){
			file.copy(from = chunked.full.names[[i]][j], to = file.path(root.dir, out.dir, paste0("reader_", i), paste0(id[j], "_", chunked.names[[i]][j])))
		})
	}
}

