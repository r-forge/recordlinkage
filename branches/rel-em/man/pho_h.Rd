\encoding{latin1}

\name{pho_h}
\alias{pho_h}

\title{Phonetic Code}
\description{
  Interface to the phonetic coding function by \enc{Jörg}{Joerg} Michael
  (see references).
}
\usage{
pho_h(str)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{str}{A character vector or matrix.}
}
\details{
  Translates it argument to a phonetic coding. Intended for German language.
}

\value{
  A character vector or matrix with the same size and dimensions as \code{str}.
}

\references{\enc{Jörg}{Joerg} Michael, \enc{Doppelgänger}{Doppelgaenger} gesucht
  -- Ein Programm \enc{für}{fuer} kontextsensitive phonetische Textumwandlung,
  in: c't 1999, No. 25, pp. 252--261. 
  The Source code is published at http://www.heise.de/ct/ftp/99/25/252/.
}
\author{Andreas Borg (R interface only)}

\seealso{\code{\link{jarowinkler}} for string comparison.}


\keyword{misc}
