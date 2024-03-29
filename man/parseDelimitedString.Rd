% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parseDelimitedString.R
\name{parseDelimitedString}
\alias{parseDelimitedString}
\title{parses a delimited string looking for standard set of delimiters (excludes "-")}
\usage{
parseDelimitedString(string, position = 1)
}
\arguments{
\item{string}{string with any number of delimiters ("," "." ";" ":")}

\item{position}{parsed element number to return}
}
\value{
parsed element number from original string
}
\description{
parses a delimited string looking for standard set of delimiters (excludes "-")
}
