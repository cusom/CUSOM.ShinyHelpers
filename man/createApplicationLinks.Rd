% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createApplicationLinks.R
\name{createApplicationLinks}
\alias{createApplicationLinks}
\title{Utility function to create application link buttons}
\usage{
createApplicationLinks(linkData)
}
\arguments{
\item{linkData}{A dataframe containing the following required columns
label - string - label to be used for link button
imageURL - string - background image to use for link button
link - string - link for onclick event linked to button
IsCurrentApplication - boolean - indicates whether the
row is for the current application}
}
\value{
shiny div containing shiny action button objects
}
\description{
Utility function to create application link buttons
}
