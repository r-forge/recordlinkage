# rpairs    Record Pairs, class RecLinkPairs
# method    Classification method, one of "em", "rpart", "bagging", "boosting", "epilink", "svm"
# control   RecLink.control object with parameters controlling the method
# train     Training data. If missing, data will be selected via unsupervised clustering
# n_train   Number of record pairs to use for training. Either this number of pairs
#           will be randomly selected from train or selected from rpairs via clustering 

#classify(rpairs, method, train=F, n_train=ifelse(train==F,nrow(rpairs$data)/10,nrow(train)),control)
classify = function (rpairs, method)
{
    # Konsistenz prüfen...
    # Trainingsdaten
#     if (!isFALSE(train))
#     {
#         n_train=min(n_train, nrow(train))
#         s=sample(1:nrow(train), n_train)
#         train=train[s,]
#     } else
#     {
#         # Methode zum Clustering aufrufen
#     }
    
    # Methode aufrufen

    if (method!="em")
    {
#         train=gensamples(rpairs,num_non=nrow(rpairs$data)/10,des_prop=0.1)
#         trainpairs=train
#         trainpairs$pairs=rbind(train$slinks,train$snonlinks)
#         trainpairs$is_match=c(rep(1,nrow(train$slinks)),rep(0,nrow(train$snonlinks)))
#         res=classfun(train=trainpairs,valid=rpairs)
        classfun=switch(method,epilink=classify.epilink,svm=classify.svm) # ergänzen!    
        if (is.null(classfun)) stop ("Illegal method")
        rpairs=gensamples(rpairs,num_non=nrow(rpairs$data)/10,des_prop=0.1)
        train=rpairs
        train$pairs=rbind(rpairs$slinks,rpairs$snonlinks)
        train$is_match=c(rep(1,nrow(rpairs$slinks)),rep(0,nrow(rpairs$snonlinks)))
        valid=rpairs
        valid$pairs=rpairs$evals
        res=classfun(train=train,valid=valid)
        rpairs$prediction=res$prediction
        class(rpairs)="RecLinkResult"
        return (rpairs)
    } else # method=="em"
    {
        return (classify.em(rpairs))
    }

}

# classify.em()
# {
#     # Schritte trennen?
#     emWeights()
#     emClassify()
# 
# }
# 
# 
# 
# classify.cart
# 
# classify.epilink
# 
# 
# # Control functions
# 
# em.control(tol=0.00001, m=0.97, my=Inf, ny=Inf)
# {
#     list(tol, m, my, ny)
# }
# 
# cart.control # wie rpart.control? Für alle cart-Varianten gleich?
# 
# epilink.control(my=Inf, ny=Inf)
# {
#     list(my, ny)
# }
# 
