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

article_selection <- function(full.names, duplicate_index, ratio = .2){
	set.seed(89)
	n <- floor(ratio * length(full.names))
	select_ind <- sample(duplicate_index, n)
	return(select_ind)
}

assign_articles_to_readers <- function(select_ind, number_of_readers, out.dir){
	selected.pdfs <- full.names[select_ind]
	chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
	chunked.pdfs <- chunk2(selected.pdfs, number_of_readers)
	for (i in 1:number_of_readers){
		dir.create(file.path(root.dir, out.dir, paste0("reader_", i)))
		lapply(chunked.pdfs[[i]], function(pdf){
			file.copy(from = pdf, to = file.path(root.dir, out.dir, paste0("reader_", i)))
		})
	}
}

