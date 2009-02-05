\name{RLdata}
\docType{data}
\alias{RLdata500}
\alias{RLdata10000}
\title{Test data for Record Linkage}
\description{
  These tables contain artificial personal data of for the 
  evaluation of Record Linkage procedures. 
}
\usage{RLdata500}
\format{A character matrix. Each row represents one record, with the following
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
}
\source{Generated with the data generation component of Febrl (Freely 
  Extensible Biomedical Record Linkage), version 0.3. See 
  http://datamining.anu.edu.au/projects/linkage.html for details.}

\author{Andreas Borg}

\keyword{datasets}
