printResults <- function (object, show="all", show_prediction=T, sort="prediction")
{
    ids=switch(show,links=object$prediction,nonlinks=!object$prediction,
               possible=is.na(object$prediction),TRUE)
    #print(ids)               
    result=merge(merge(object$valid[,1:2],cbind(seq(1,nrow(object$data)), object$data),
                by.x=1, by.y=1),
                cbind(seq(1:nrow(object$data)),object$data),by.x=2,by.y=1)
    if (show_prediction)
        result$prediction=object$prediction
    # Sortiermechanismus nochmal überdenken, man hat ja noch keine Vorstellung
    # von der Paartabelle!
#     if (is.numeric(sort_cols))
#         sort_cols=result[,sort+2] # +2 because of ids
#     if (is.logical)
#     if (!is.list(sort_cols))
#         sort_cols=list(sort_cols)
#    return(result[do.call(order,sort_cols),])
    return(result[ids,])
}

        
