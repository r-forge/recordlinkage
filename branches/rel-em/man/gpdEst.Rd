\name{gpdEst}
\Rdversion{1.1}
\alias{gpdEst}

\title{
  Estimate Threshold from Pareto Distribution
}
\description{
  
}
\usage{
gpdEst(Wdata, thresh = -Inf, quantil = 0.95, scale = NA, shape = NA)
}

\arguments{
  \item{Wdata}{
    A numeric vector representing weights of record pairs.
  }
  \item{thresh}{
    Threshold for exceedances.
  }
  \item{quantil}{
    A real number between 0 and 1. The desired quantil.
  }
}


\details{
  The weights that exceed \code{thresh} are fitted to a 
  generalized Pareto distribution (GPD). The estimated parameters \code{shape}
  and \code{shape} are used to calculate a classification threshold by the
  formula
  \deqn{thresh+\frac{scale}{shape}((\frac{n}{k}(1-quantil))^{-shape} -1)}{
  thresh+scale/shape*((n/k*(1-quantil))^(-shape) -1)}  
  where \eqn{n} is the total number of weights and \eqn{k} the number of
  exceedances.
}
\value{
  A real number representing the resulting classification threshold.
}

\author{
  Murat Sariyar
}


\seealso{
  \code{\link{getParetoThreshold}} for user-level function
}

\keyword{models}
\keyword{misc}
