\name{RLdata}
\docType{data}
\alias{RLdata500}
\alias{RLdata10000}
\alias{identity.RLdata500}
\alias{identity.RLdata10000}

\title{Test data for Record Linkage}

\description{
  These tables contain artificial personal data of for the 
  evaluation of Record Linkage procedures. Some records have been duplicated
  with randomly generated errors. \code{RLdata500} contains 50 duplicates,
  \code{RLdata10000} 1000 duplicates.
}

\usage{RLdata500
  RLdata10000
  identity.RLdata500
  identity.RLdata10000}

\format{\code{RLdata500} and \code{RLdata10000} are character matrix with 
  500 and 10000 records. Each row represents one record, with the following
  columns:
  \describe{
    \item{fname\_c1}{First name, first component}
    \item{fname\_c2}{First name, second component}
    \item{lname\_c1}{Last name, first component}
    \item{lname\_c2}{Last name, second component}
    \item{by}{Year of birth}
    \item{bm}{Month of birth}
    \item{bd}{Day of birth}
  }
  \code{identity.RLdata500} and \code{identity.RLdata10000} are integer vectors
  representing the true record
  ids of the two data sets. Two records are duplicates, if and only if their
  corresponding values in the identity vector agree.
}

\source{Generated with the data generation component of Febrl (Freely 
  Extensible Biomedical Record Linkage), version 0.3. See 
  http://datamining.anu.edu.au/projects/linkage.html for details.}

\author{Andreas Borg}

\keyword{datasets}
