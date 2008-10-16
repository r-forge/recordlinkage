# Print functions for Record Linkage Objects

print.RecLinkPairs <- function(x)
{
    cat("Record Linkage Comparision Object\n") 
    cat(sprintf("%d records",nrow(x$data)),"\n")
    cat(sprintf("%d record pairs",nrow(x$pairs)),"\n")
}

print.RecLinkResult <- function(x)
{
    cat("Record Linkage Result Object\n") 
    cat(sprintf("%d records",nrow(x$data)),"\n")
    cat(sprintf("%d record pairs",nrow(x$pairs)),"\n")  
    cat(sprintf("%d links detected",length(which(x$prediction==T))),"\n")
    cat(sprintf("%d possible links detected",length(which(is.na(x$prediction)))),"\n")
    cat(sprintf("%d non-links detected",length(which(x$prediction==F))),"\n")
}

links.RecLinkResult <- function(x)
{
    return  (cbind(x$data[(x$pairs[which(x$prediction),,drop=F])[,1],,drop=F],x$data[(x$pairs[which(x$prediction),,drop=F])[,2],,drop=F]))
}

nonlinks.RecLinkResult <- function(x)
{
    return  (cbind(x$data[(x$pairs[which(!x$prediction),,drop=F])[,1],,drop=F],x$data[(x$pairs[which(!x$prediction),,drop=F])[,2],,drop=F]))
}

possiblelinks.RecLinkResult <- function(x)
{
    return  (cbind(x$data[(x$pairs[which(is.na(x$prediction)),,drop=F])[,1],,drop=F],x$data[(x$pairs[which(is.na(x$prediction)),,drop=F])[,2],,drop=F]))
}

