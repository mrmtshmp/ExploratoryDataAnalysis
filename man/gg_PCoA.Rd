% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggPCoA.R
\name{gg_PCoA}
\alias{gg_PCoA}
\title{PCoA plot via cmdscale}
\usage{
gg_PCoA(
  distmat,
  groups,
  ggplot_theme = summ_plot_theme,
  axes = c("CS1", "CS2"),
  k_pc = c(1, 2),
  overlay = FALSE,
  labels = "sample_ID",
  title = "PCoA plot using by MDS",
  legend.name = "Cluster",
  jitter.v = 0.3,
  jitter.h = 0.3,
  size = 2,
  alpha = 0.7
)
}
\arguments{
\item{distmat}{<object; input data; mandatory> An object with dist-class.}

\item{groups}{<character; output> A varible used forcolor coding.}

\item{ggplot_theme}{<object; proccessing>}

\item{axes}{<character strings; output; prefix = c("CS11","CS2")>  Labels for x and y axes.}

\item{k_pc}{<numeric>}

\item{overlay}{<logical; output; prefix =FALSE> Print subject IDs on respective plots?}

\item{labels}{<; processing; prefix =2> Colour brewer setting}

\item{title}{<object; input data; mandatory> A data.frame with features as column and subjects as rows.}

\item{legend.name}{<object; input data> A data.frame with 2 column of a feature and subject ID. This ID must be identical with of count_table.}

\item{jitter.v}{<numeric; proccessing; prefix=0.3>}

\item{jitter.h}{<numeric; proccessing; prefix=0.3>}

\item{size}{<numeric; proccessing; prefix=2>}

\item{alpha}{<numeric; proccessing; prefix=0.7>}
}
\description{
PCoA plot via cmdscale
}
