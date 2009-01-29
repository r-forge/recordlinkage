\name{emClassify}
\alias{emClassify}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
emClassify(rpairs, my = Inf, ny = Inf, threshold_upper = Inf, threshold_lower = threshold_upper)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rpairs}{ ~~Describe \code{rpairs} here~~ }
  \item{my}{ ~~Describe \code{my} here~~ }
  \item{ny}{ ~~Describe \code{ny} here~~ }
  \item{threshold_upper}{ ~~Describe \code{threshold_upper} here~~ }
  \item{threshold_lower}{ ~~Describe \code{threshold_lower} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (rpairs, my=Inf, ny=Inf,threshold_upper=Inf, 
                        threshold_lower=threshold_upper)
{    
    o=order(rpairs$W,decreasing=T) # order Weights decreasing

    # if no threshold was given, compute them according to the error bounds
    if (missing(threshold_upper) && missing(threshold_lower))
    {
      FN=rev(cumsum(rev(rpairs$M[o]))) 
      FP=cumsum(rpairs$U[o])
      if (my==Inf && ny==Inf)
      {
          # no error bound given: minimize overall error
          cutoff_upper=which.min(c(0,FP)+c(FN,0))-1
          if (length(cutoff_upper)==0)
              cutoff_upper=0
          cutoff_lower=cutoff_upper
          
      } else if (my==Inf)
      {  
          # only rate of false matches relevant
          cutoff_lower=head(which(FN<=ny),1)
          if (length(cutoff_lower)==0)
              cutoff_lower=length(o)
          cutoff_upper=cutoff_lower
      
      } else if (ny==Inf)
      {
          # only rate of false non-matches relevant
          cutoff_upper=tail(which(FP<=my),1)
          cutoff_lower=cutoff_upper
      } else
      {
          # both error bounds relevant
          cutoff_upper=tail(which(FP<=my),1)
          cutoff_lower=head(which(FN<=ny),1)
          if (length(cutoff_upper)==0)
              cutoff_upper=0
          if (length(cutoff_lower)==0)
              cutoff_lower=length(o)
          if (cutoff_lower<cutoff_upper)
          {
              cutoff_upper=which.min(c(0,FP)+c(FN,0))-1
              cutoff_lower=cutoff_upper
          }
      } 
      print("Threshold berechnen und Klassifikation zuweisen")
      threshold_upper=rpairs$W[o][cutoff_upper]
      threshold_lower=rpairs$W[o][cutoff_lower]
    } # end if
     
    prediction=as.logical(rep(NA,nrow(rpairs$valid)))
    prediction[rpairs$Wdata>=threshold_upper]=T
    prediction[rpairs$Wdata<threshold_lower]=F
    
    ret=rpairs # keeps all components of rpairs
    ret$prediction=prediction
	ret$threshold=threshold_upper
    class(ret)="RecLinkResult"
    return(ret)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
