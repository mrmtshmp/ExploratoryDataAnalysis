% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mf.complication_matrix.R
\name{mf.comlication_matrix}
\alias{mf.comlication_matrix}
\title{Complication matrix.}
\usage{
mf.comlication_matrix(data, col_selector = "^sAB\\\\.[^others$]|^id$|^season$")
}
\arguments{
\item{data}{A data.frame-class object. Subjects' IDs have to be contained in a column named 'id'.}

\item{col_selector}{A regex string to specify names of cols to be analyzed.}
}
\description{
Complication matrix.
}
