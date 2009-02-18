summary.RecLinkData <- function(object,...)
{
#    cat("Record Linkage Data Object\n\n") 
    if (object$type=="linkage")
    {
        cat("\nLinkage Project\n\n")
        cat(sprintf("%d records in data set 1",nrow(object$data1)),"\n")
        cat(sprintf("%d records in data set 2",nrow(object$data2)),"\n")
    }
    else
    {
        cat("\nDeduplication Project\n\n")
        cat(sprintf("%d records",nrow(object$data)),"\n")
    }

    cat(sprintf("%d training pairs",nrow(object$train)),"\n")
    cat(sprintf("%d validation pairs",nrow(object$valid)),"\n")
    cat("\n")
    # the expression "length(which(..." is needed to eliminate NAs
    if (nrow(object$train) > 0)
	{
		cat(sprintf("%d matches in training set\n",
	        length(which(object$train$is_match==T))))
	    cat(sprintf("%d non-matches in training set\n",
	        length(which(object$train$is_match==F))))
	    cat("\n")
	}
	if (nrow(object$valid) > 0)
	{
	    cat(sprintf("%d matches in validation set\n",
	        length(which(object$valid$is_match==T))))
	    cat(sprintf("%d non-matches in validation set\n",
	        length(which(object$valid$is_match==F))))
	    if (any(is.na(object$valid$is_match)))
	        cat(sprintf("%d pairs with unknown status in validation set\n",
	            sum(is.na(object$valid$is_match))))
	}
	if (!is.null(object$Wdata))
	{
		cat("\n")
		cat("Weight distribution:\n\n")
		h=hist(object$Wdata,plot=F)
		print(h$breaks)
		print(h$counts)
	}
}


# obsolet: abgelöst von RecLinkData

# summary.RecLinkPairs <- function(object,...)
# {
#     cat("Record Linkage Comparision Object\n\n") 
#     cat(sprintf("%d records",nrow(object$data)),"\n")
#     cat(sprintf("%d record pairs",nrow(object$pairs)),"\n")
#     if (!is.null(object$identity))
#     {
#         cat("\n")
#         is_match=object$identity[object$pairs$id1]==object$identity[object$pairs$id2]
#         # the expression "length(which(..." is needed to eliminate NAs
#         cat(sprintf("%d matches\n",length(which(is_match==T))))
#         cat(sprintf("%d non-matches\n",length(which(is_match==F))))
#         if (any(is.na(is_match)))
#             cat(sprintf("%d pairs with unknown status\n",sum(is.na(is_match))))
#     }
# }

summary.RecLinkResult <- function (object, ...)
{
    summary.RecLinkData(object,...)
    cat("\n")
    cat(sprintf("%d links detected",length(which(object$prediction=="L"))),"\n")
    cat(sprintf("%d possible links detected",length(which(object$prediction=="P"))),"\n")
    cat(sprintf("%d non-links detected",length(which(object$prediction=="N"))),"\n")
 
#     if (!is.null(object$is_match))
#     {
#         is_match=object$is_match
#     } else if (!is.null(object$identity))
#     {
#         is_match=object$identity[object$pairs$id1]==object$identity[object$pairs$id2]
#     } else
#     {
#         return ()
#     }
    cat("\n")
#     TP=sum(is_match & object$prediction) # true positive
#     FP=sum(!is_match & object$prediction) # false positive
#     TN=sum(!is_match & !object$prediction) # true negative
#     FN=sum(is_match & !object$prediction) # false negative
    TP=length(which(object$valid$is_match & object$prediction=="L")) # true positive
    FP=length(which(!object$valid$is_match & object$prediction=="L")) # false positive
    TN=length(which(!object$valid$is_match & object$prediction=="N")) # true negative
    FN=length(which(object$valid$is_match & object$prediction=="N")) # false negative
    
    alpha=FN/(TP+FN)
    beta=FP/(TN+FP)
    accuracy=(TP+TN)/(TP+TN+FP+FN)
    cat(sprintf("alpha error: %f\n",alpha))
    cat(sprintf("beta error: %f\n",beta))
    cat(sprintf("accuracy: %f\n",accuracy))
    cat("\n\n")
    cat("Classification table:\n\n")
#     print(table(as.logical(object$valid$is_match),as.logical(object$prediction),
#           dnn=list("true status","prediction"),exclude=NULL))
    print(table(as.logical(object$valid$is_match),object$prediction,
          dnn=list("true status","classification"),useNA="ifany"))
}