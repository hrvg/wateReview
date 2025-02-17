% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/network_analysis.R
\name{VizSpots}
\alias{VizSpots}
\title{Produces a chord diagram visualization with country cluster colors and topics categories}
\usage{
VizSpots(
  m,
  scaled = FALSE,
  cluster_color = TRUE,
  NSF_general_color = TRUE,
  type = "theme",
  sort_topic = TRUE,
  topic_threshold = 0.75,
  reorder_cluster = FALSE
)
}
\arguments{
\item{m}{matrix, weighted adjacency matrix with rows indicating origin and columns indicating destination}

\item{scaled}{logical, default to FALSE}

\item{cluster_color}{logical, default to TRUE}

\item{NSF_general_color}{logical, default to TRUE}

\item{type}{character, default to "theme"}

\item{sort_topic}{logical, default to TRUE}

\item{topic_threshold}{numeric, a percentile to cut the display of topic names}

\item{reorder_cluster}{logical, default to FALSE, controls if the origin is re-ordered by cluster}
}
\value{
a chord diagram figure
}
\description{
Produces a chord diagram visualization with country cluster colors and topics categories
}
