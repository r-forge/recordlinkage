mygllm <- function(y,s,X,maxit=1000, tol=0.00001, E=rep(1,length(s)),dec_int_tol=F)
{
   out <- .C("mygllm", as.integer(y), as.integer(s-1), 
             X=as.double(cbind(X,rep(0,nrow(X)))),
             as.integer(maxit), as.double(tol), estimate=as.double(E),
             length(s),length(y),ncol(X),as.logical(dec_int_tol),
             F,    
             PACKAGE="RecordLinkage")
   return(out$estimate)
}
