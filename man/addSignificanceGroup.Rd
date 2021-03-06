% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AddSignificanceGroup.R
\name{addSignificanceGroup}
\alias{addSignificanceGroup}
\title{Function to add 3 group significance group label to fold change dataframe}
\usage{
addSignificanceGroup(.data, foldChangeVar, pValueVar, threshold)
}
\arguments{
\item{.data}{fold change dataframe (x = fold change + y = p value )}

\item{foldChangeVar}{name of fold change column}

\item{pValueVar}{name of p value column}

\item{threshold}{p value threshold defaults to 0.05}
}
\value{
returns labels for signficance groups (3 - up, down, not significant)
}
\description{
Function to add 3 group significance group label to fold change dataframe
}
