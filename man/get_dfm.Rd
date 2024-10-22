% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text_mining.R
\name{get_dfm}
\alias{get_dfm}
\title{Retrieve or create a document-feature matrix (dfm) from hard coded options relevant to current project}
\usage{
get_dfm(
  dfm_file = "F:/hguillon/research/exploitation/R/latin_america/data/obj_dfm.Rds",
 
    corpus_file = "F:/hguillon/research/exploitation/R/latin_america/data/english_corpus.Rds",
  text_col = "raw"
)
}
\arguments{
\item{dfm_file}{Path to a dfm file, if file exists, file is read, if not dfm is computed from corpus}

\item{corpus_file}{path to a corpus data.frame with text in text column}

\item{text_col}{text column in the corpus data.frame}
}
\value{
document-feature matrix extracted
}
\description{
Retrieve or create a document-feature matrix (dfm) from hard coded options relevant to current project
}
