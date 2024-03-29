% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetStatTestByKeyGroup.R
\name{getStatTestByKeyGroup}
\alias{getStatTestByKeyGroup}
\title{Calculate statistical test between 2 groups for each key value in a dataframe}
\usage{
getStatTestByKeyGroup(
  .data,
  id,
  key,
  response,
  independentVariable,
  baselineLabel,
  testMethod,
  ...
)
}
\arguments{
\item{.data}{A dataframe}

\item{id}{A string or numeric column - represents a unique
observation within each key/group combination. PersonId, LabId, etc}

\item{key}{A string or numeric column - key value for dataframe.
Statistics will be computed between groups for each key value}

\item{response}{A numeric column - numerical value to use
with statitical test between groups.}

\item{independentVariable}{A string column indicating group membership - should be binary.}

\item{baselineLabel}{independentVariable label used for fold change comparison / calculation}

\item{testMethod}{a string - indicating which statisical
test to perform. One of either ks.test, t.test, or wilcox.test}

\item{...}{dots - accomodate additional arguments
function is called as parent router}
}
\value{
dataframe containing resulting p.values for each key/group stat test
}
\description{
Calculate statistical test between 2 groups for each key value in a dataframe
}
