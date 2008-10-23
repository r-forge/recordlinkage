
# Eingabe: 
#   - Trainingsdaten (generiert)
#   - Validierungsdaten (echt)

#     f <- function(rpairs,...)
#     {
#         res=classify.single.svm(rpairs,...)
#         tab=tbl(res)
#         accuracy=(tab[1,1]+tab[2,2])/sum(tab)
#         return(1/accuracy)
#     }
# 
# classify.svm = function (rpairs)
# {
#     library(e1071)
#     library(gafit)
#     ret=rpairs
#     rpairs$train[is.na(rpairs$train)]=0
#     rpairs$valid[is.na(rpairs$valid)]=0
# 
#     s=sample(1:nrow(rpairs$train),0.7*nrow(rpairs$train))
#     # fitpairs has to reside in the global environment during optimization
#     # (see help on gafit)
#     assign("fitpairs",rpairs,envir=globalenv())
#     fitpairs$train=rpairs$train[s,]
#     fitpairs$valid=rpairs$train[-s,]
#     e=expression(f(fitpairs,gamma=g,epsilon=e,cost=c))
#     fitted=gafit(e,start=list(g=1,e=0.1,c=1))
#     rm(fitpairs)
#     return (classify.single.svm(rpairs,gamma=fitted$g,epsilon=fitted$e,cost=fitted$c))
# }

classify.svm = function(rpairs,...)
{
    ret=rpairs
    rpairs$train[is.na(rpairs$train)]=0
    rpairs$valid[is.na(rpairs$valid)]=0
    x=as.matrix(rpairs$train[,-c(1,2,ncol(rpairs$train))])
    y=as.matrix(as.integer(rpairs$train$is_match))
    model=svm(y ~ x, kernel="radial",type="C-classification",...)
    x=as.matrix(rpairs$valid[,-c(1,2,ncol(rpairs$valid))])
    predict=predict(model, newdata=x)       
    ret$prediction=predict
    return(ret)

}
