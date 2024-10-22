% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mlr_predictions.R
\name{get_binaryRelevanceLearners}
\alias{get_binaryRelevanceLearners}
\title{Convience legacy function to create binary relance wrappers from MLR}
\usage{
get_binaryRelevanceLearners(
  lrn = "classif.svm",
  trainingData,
  validationHumanReadingDTM,
  scale_type = "location",
  aggregated_labels = FALSE
)
}
\arguments{
\item{lrn}{a base learner, default to "classif.svm"}

\item{trainingData}{data.frame of training data}

\item{validationHumanReadingDTM}{document-term matrix from human reading}

\item{scale_type}{One of "location", "spatial", "temporal", default to "location"}

\item{aggregated_labels}{logical}
}
\value{
a list of learners
}
\description{
Convience legacy function to create binary relance wrappers from MLR
}
