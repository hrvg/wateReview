% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clustering.R
\name{get_optimk}
\alias{get_optimk}
\title{Display the optimum number of cluster for a given clustering method}
\usage{
get_optimk(df, method = "kmeans", kmax = 10, gap = FALSE)
}
\arguments{
\item{df}{a data.frame}

\item{method}{character, one of "kmeans", "hierarchical"}

\item{kmax}{numeric, the maximum number of cluster to test}

\item{gap}{logical, controls if the gap statistics have to be computed, default to \code{FALSE}}
}
\value{
a \code{gridExtra} object with \code{ggplot2} plots
}
\description{
Display the optimum number of cluster for a given clustering method
}
