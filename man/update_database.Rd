% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manual_download.R
\name{update_database}
\alias{update_database}
\title{Update the database by marking the manuall downloaded articles as \verb{in corpus}}
\usage{
update_database(language, language_dfs)
}
\arguments{
\item{language}{character, one of "english", "spanish", "portuguese"}

\item{language_dfs}{list of data.frame with the information from the query and the corpus collection}
}
\value{
udpated \code{language_dfs} list of data.frame with the information from the query and the corpus collection
}
\description{
Update the database by marking the manuall downloaded articles as \verb{in corpus}
}
