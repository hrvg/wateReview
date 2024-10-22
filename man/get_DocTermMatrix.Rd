% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_loading.R
\name{get_DocTermMatrix}
\alias{get_DocTermMatrix}
\title{read document-term matrix created by the text mining code}
\usage{
get_DocTermMatrix(
  dtm_file = system.file("extdata", "obj_dtm_from_dfm_country.Rds", package =
    "wateReview")
)
}
\arguments{
\item{dtm_file}{path to saved document-term matrix}
}
\value{
document-term matrix
}
\description{
read document-term matrix created by the text mining code
}
