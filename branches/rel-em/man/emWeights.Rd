\name{emWeights}
\alias{emWeights}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
emWeights(rpairs, m = 0.97, cutoff = 0.95, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rpairs}{ ~~Describe \code{rpairs} here~~ }
  \item{m}{ ~~Describe \code{m} here~~ }
  \item{cutoff}{ ~~Describe \code{cutoff} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
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
function (rpairs, m=0.97,cutoff=0.95,...)
{
    library(e1071)
# t0=proc.time()
# print("Datenvorbereitung")
    pairs=rpairs$valid
    # ids und Matchingstatus rausnehmen
    pairs=pairs[,-c(1,2,ncol(pairs))]
# print(proc.time()-t0)
# t0=proc.time()
    pairs=as.matrix(pairs)
    pairs[is.na(pairs)]=0
# print("Fuzzy umrechnen")
    is_fuzzy=!all(is.element(pairs,0:1))
    if (is_fuzzy)
    {
        pairs_fuzzy=pairs
        pairs=as.array((pairs>=cutoff)*1)
    }
# print(proc.time()-t0)
# t0=proc.time()

# print("Patterns zählen, em vorbereiten")
    n_data=nrow(pairs)  
    observed_count=countpattern(pairs)
    n_attr=ncol(pairs)
    patterns=bincombinations(n_attr)  # Liste der Patterns
    x=c(rep(0,nrow(patterns)),rep(1,nrow(patterns)))
    s=c(1:length(observed_count), 1:length(observed_count))
    i=rep(1,nrow(patterns)) # Intercept
    X=cbind(i,x,rbind(patterns,patterns),rbind(patterns,patterns)*x) # Design Matrix
# print(proc.time()-t0)
# t0=proc.time()

# print("Häufigkeiten schätzen")
    u=rpairs$frequencies    
    m=0.97
    # Ad-hoc-Schätzung für Anteil an Matchen (Faktor 0.1 relativ beliebig)
    prob_M=1/sqrt(n_data)*0.1
    # Anzahl schätzen für Matche
    init_M=apply(patterns,1,function(a) prod(a*m+(1-a)*(1-m))*n_data*prob_M)
    init_U=apply(patterns,1,function(a) prod(a*u+(1-a)*(1-u))*n_data*(1-prob_M))
    expected_count=c(init_U,init_M)
# print(proc.time()-t0)
# t0=proc.time()

# print("EM ausführen")   
    res=mygllm(observed_count,s,X,E=expected_count,...)
# print(proc.time()-t0)
# t0=proc.time()

# print("Der Rest")
    n_patterns=length(res)/2

    # Anteil Matche/Non_Matche in einem Pattern
    matchrate=res[(n_patterns+1):(2*n_patterns)]/res[1:n_patterns]
    #matchrate=round(res[(n_patterns+1):(2*n_patterns)])/round(res[1:n_patterns])
#    o=order(matchrate,res[(n_patterns+1):(2*n_patterns)],decreasing=T)

    n_matches=sum(res[(n_patterns+1):(2*n_patterns)])
    n_nonmatches=sum(res[1:n_patterns])
    U=res[1:n_patterns]/n_nonmatches
    M=res[(n_patterns+1):(2*n_patterns)]/n_matches
    W=log(M/U, base=2)
    indices=colSums(t(pairs)*(2^(n_attr:1-1)))+1    
    ret=rpairs # keeps all components of rpairs
    ret$M=M
    ret$U=U
    ret$W=W
    ret$Wdata=W[indices]
    ret$PM=n_matches/n_data
    ret$res=res
    if (is_fuzzy)
    {
        str_weights=apply(pairs_fuzzy^pairs,1,prod)
        ret$Wdata=ret$Wdata+log(str_weights, base=2)
    } 
#    class(ret)="RecLinkResult"
# print(proc.time()-t0)
cat("\n")
    return(ret)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
