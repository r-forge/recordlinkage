\name{jarowinkler}
\alias{jarowinkler}
\alias{jaro}
\alias{winkler}
\title{Compute Jaro-Winkler string metric}
\description{
   Computes Jaro-Winkler string metric for pairs of strings.
   	}
\usage{jarowinkler(str1, str2, W_1=1/3, W_2=1/3, W_3=1/3, r=0.5)}

\arguments{
   \item{str1,str2}{Two character vectors to compare.}
   \item{W_1,W_2,W_3}{Adjustable weights. See details.}
   \item{r}{Maximum radius of transpositions, as fraction of the length of the 
            shorter string.}
   }
   
\details{Computes the similarity of strings according to the Jaro-Winkler
  comparator. For the meaning of \code{W_1}, \code{W_2}, \code{W_3} and 
  \code{r} see the referenced article. For most applications, the default 
  values are okay.
  The function is vectorized and supports recycling. If both \code{str1} and
  \code{str2} are arrays, their dimensions must agree.}

\value{A numeric vector with similarity values in the interval 
  \eqn{[0,1]}{[0,1]}. The elements of the shorter vector of \code{str1} and
  \code{str2} are recycled as necessary.
  }

\references{Winkler, W.E.: String Comparator Metrics and Enhanced Decision
Rules in the Fellegi-Sunter Model of Record Linkage. In: Proceedings
of the Section on Survey Research Methods, American Statistical Association
(1990), S. 354-369}

\author{Andreas Borg}
\keyword{misc}

