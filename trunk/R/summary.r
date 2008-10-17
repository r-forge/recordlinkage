summary.RecLinkPairs <- function(object,...)
{
    cat("Record Linkage Comparision Object\n\n") 
    cat(sprintf("%d records",nrow(object$data)),"\n")
    cat(sprintf("%d record pairs",nrow(object$pairs)),"\n")
    if (!is.null(object$identity))
    {
        cat("\n")
        is_match=object$identity[object$pairs$id1]==object$identity[object$pairs$id2]
        # the expression "length(which(..." is needed to eliminate NAs
        cat(sprintf("%d matches\n",length(which(is_match==T))))
        cat(sprintf("%d non-matches\n",length(which(is_match==F))))
        if (any(is.na(is_match)))
            cat(sprintf("%d pairs with unknown status\n",sum(is.na(is_match))))
    }
}

summary.RecLinkResult <- function (object, ...)
{
    summary.RecLinkPairs(object,...)
    cat("\n")
    cat(sprintf("%d links detected",length(which(object$prediction==T))),"\n")
    cat(sprintf("%d possible links detected",length(which(is.na(object$prediction)))),"\n")
    cat(sprintf("%d non-links detected",length(which(object$prediction==F))),"\n")
 
    if (!is.null(object$is_match))
    {
        is_match=object$is_match
    } else if (!is.null(object$identity))
    {
        is_match=object$identity[object$pairs$id1]==object$identity[object$pairs$id2]
    } else
    {
        return ()
    }
    cat("\n")
#     TP=sum(is_match & object$prediction) # true positive
#     FP=sum(!is_match & object$prediction) # false positive
#     TN=sum(!is_match & !object$prediction) # true negative
#     FN=sum(is_match & !object$prediction) # false negative
    TP=length(which(is_match & object$prediction)) # true positive
    FP=length(which(!is_match & object$prediction)) # false positive
    TN=length(which(!is_match & !object$prediction)) # true negative
    FN=length(which(is_match & !object$prediction)) # false negative
    
    alpha=FN/(TP+FN)
    beta=FP/(TN+FP)
    accuracy=(TP+TN)/(TP+TN+FP+FN)
    cat(sprintf("alpha error: %f\n",alpha))
    cat(sprintf("beta error: %f\n",beta))
    cat(sprintf("accuracy: %f\n",accuracy))
    cat("\n\n")
    cat("Classification table:\n\n")
    print(table(as.logical(is_match),as.logical(object$prediction),
          dnn=list("true status","prediction"),exclude=NULL))
}