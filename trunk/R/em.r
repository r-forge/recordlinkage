library(e1071)
library(mygllm)
# Arguments:
#
#   rpairs  data pairs (class RecLinkPairs)
#   m       probability for an error (m-probability), either one value for
#           all attributes or a vector with distinct values
emWeights <- function (rpairs, m=0.97)
{
    pairs=rpairs$pairs
    # ids rausnehmen
    pairs=pairs[,-c(1:2)]
    pairs[is.na(pairs)]=0
    pairs=array(as.integer(pairs>=0.95),dim=dim(pairs))


    n_data=nrow(pairs)
    observed_count=countpattern(pairs)
    n_attr=ncol(pairs)
    patterns=bincombinations(n_attr)  # Liste der Patterns
    x=c(rep(0,nrow(patterns)),rep(1,nrow(patterns)))
    s=c(1:length(observed_count), 1:length(observed_count))
    i=rep(1,nrow(patterns)) # Intercept
    X=cbind(i,x,rbind(patterns,patterns),rbind(patterns,patterns)*x) # Design Matrix

    u=rpairs$frequencies    
    m=0.97
    # Ad-hoc-Schätzung für Anteil an Matchen (Faktor 0.1 relativ beliebig)
    prob_M=1/sqrt(n_data)*0.1
    # Anzahl schätzen für Matche
    init_M=apply(patterns,1,function(a) prod(a*m+(1-a)*(1-m))*n_data*prob_M)
    init_U=apply(patterns,1,function(a) prod(a*u+(1-a)*(1-u))*n_data*(1-prob_M))
    expected_count=c(init_U,init_M)
   
    res=mygllm(observed_count,s,X,E=expected_count)

    n_patterns=length(res)/2

    # Anteil Matche/Non_Matche in einem Pattern
    matchrate=res[(n_patterns+1):(2*n_patterns)]/res[1:n_patterns]
    #matchrate=round(res[(n_patterns+1):(2*n_patterns)])/round(res[1:n_patterns])
    o=order(matchrate,res[(n_patterns+1):(2*n_patterns)],decreasing=T)

    n_matches=sum(res[(n_patterns+1):(2*n_patterns)])
    n_nonmatches=sum(res[1:n_patterns])
    U=res[1:n_patterns]/n_nonmatches
    M=res[(n_patterns+1):(2*n_patterns)]/n_matches
    W=log(M/U, base=2)
    ret=list()
    ret$data=rpairs$data
    ret$pairs=rpairs$pairs
    ret$M=M
    ret$U=U
    ret$W=log(M/U, base=2)
    class(ret)="RecLinkResult"
    return(ret)
}

# Arguments:
#   rpairs      weighted record pairs (output of emWeights)
#   my      error bound: # False Matches / # Found Matches
#   ny      error bound  # False Non-Matches / # Found Non-Matches
#       If an error bound is Inf, it will not be considered, meaning that
#       "possible link" will not be assigned
emClassify <- function (rpairs, my=Inf, ny=Inf)
{    
    rpairs$pairs[is.na(rpairs$pairs)]=0 # convert NAs to 0
    o=order(rpairs$W,decreasing=T) # order Weights decreasing
    n_attr=ncol(rpairs$pairs)-2 # number of attributes
    # For each record pair, compute index of corresponding pattern in the
    # table of binary combinations
    indices=colSums(t(rpairs$pairs[,-(1:2)])*(2^(n_attr:1-1)))+1    

    # FN[k]: Ratio of false non-matches if patterns FN[k..] are considered
    # as non-matches.
    # FP[k]: Ratio of false matches if patterns FN[1..k] are considered
    # as matches.  
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
    
    L_ind=o[1:cutoff_upper] # indices of detected links
    U_ind=o[cutoff_lower:length(o)] # indices of detected non-links
    # links get result TRUE, non-links FALSE, possible links NA
    prediction=as.logical(rep(NA,nrow(rpairs$pairs)))
    prediction[which(is.element(indices,L_ind))]=T
    prediction[which(is.element(indices,U_ind))]=F
    ret=list()
    ret$data=rpairs$data
    ret$pairs=rpairs$pairs
    ret$prediction=prediction
    class(ret)="RecLinkResult"
    return(ret)
}