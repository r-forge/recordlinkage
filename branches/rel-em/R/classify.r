
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

trainSupv <- function(dataset,method,use.pred=FALSE,omit.possible=TRUE,convert.na=TRUE,...)
{
	pairs=dataset$pairs[,-c(1:2)]
	if (convert.na)
		pairs[is.na(pairs)]=0
	if (use.pred)
    {
    	if (is.null(dataset$prediction))
    		stop ("No prediction vector in dataset! Call with use.pred=FALSE.")
		pairs$is_match=dataset$prediction
	} else
	{
		pairs$is_match=factor(pairs$is_match)
		levels(pairs$is_match)=c("N","L","P")
		pairs$is_match[is.na(pairs$is_match)]="P"		
	}
	# delete possible links if desired
	if (omit.possible)
		pairs=pairs[pairs$is_match!="P",]

	model=switch(method,
		svm=svm(is_match ~ .,data=pairs,type="C-classification",...),
		rpart=rpart(is_match ~ .,data=pairs,method="class",...),
		ada=ada(is_match ~ .,data=pairs,...),
		bagging=bagging(is_match ~ .,data=pairs,method="class",...),
		nnet=nnet(is_match ~ .,data=pairs,size=ncol(pairs)*2, ...),
		warning("Illegal method"))
		
	ret=list(train=dataset,model=model,method=method)
	class(ret)="RecLinkClassif"
    return(ret)

}


classifySupv <- function(model,newdata,...)
{
	ret=newdata
	if (ncol(model$train$pairs)!=ncol(newdata$pairs))
		stop ("Mismatching number of attributes!")

    x=newdata$pairs[,-c(1,2,ncol(newdata$pairs))]
	x[is.na(x)]=0

    predict=switch(model$method,
		svm=predict(model$model, newdata=x,...),       
		rpart=predict(model$model, newdata=x,type="class",...),       
		ada=predict(model$model, newdata=x,type="class",...),       
		bagging=predict(model$model, newdata=x,type="class",...),
		nnet=predict(model$model, newdata=x,type="class",...))			       
    ret$prediction=predict
    class(ret)="RecLinkResult"
    return(ret)
}


classifyUnsup <- function(dataset, method,...)
{
	if (method=="kmeans" || method=="bclust")
	{
		x=as.matrix(dataset$pairs[,-c(1,2,ncol(dataset$pairs))])
		x[is.na(x)]=0
		clust=switch(method,
			kmeans=kmeans(x,centers=2,...),
			bclust=bclust(x,...))
		y=clust$cluster
		# mark links and non-links. The cluster farther from 0 is
		# interpreted as link cluster
		link=ifelse(sum(clust$centers[1,])>sum(clust$centers[2,]),1,2)
		dataset$prediction=rep("N",length(y))
		dataset$prediction[y==link]="L"
		class(dataset)="RecLinkResult"
		return(dataset)	
	}
	stop("Illegal method!")
}


# Getrennte Funktionen für Methoden. Falls zuviel Fallunterscheidung
# nötig ist, können diese reaktiviert werden

# trainRpart <- function(dataset,use.pred=F,omit.possible=T,...)
# {
# 	library(rpart)
# 	pairs=dataset$pairs[,-c(1:2)]
# 	pairs[is.na(pairs)]=0
# 	if (use.pred)
#     {
#     	if (is.null(dataset$prediction))
#     		stop ("No prediction vector in dataset! Call with use.pred=T.")
# 		pairs$is_match=dataset$prediction
# 	} else
# 	{
# 		pairs$is_match=factor(pairs$is_match)
# 		levels(pairs$is_match)=c("N","L","P")
# 		pairs$is_match[is.na(pairs$is_match)]="P"		
# 	}
# 	# delete possible links, not suitable for training
# 	if (omit.possible)
# 		pairs=pairs[pairs$is_match!="P",]
# 
# #     model=svm(is_match ~ .,data=pairs,type="C-classification",...)
#     model=rpart(is_match ~ .,data=pairs,method="class",...)
# 
# 	ret=list(train=dataset,model=model)
# 	class(ret)="RecLinkClass"
#     return(ret)
# }
# 
# 
# trainSvm <- function(dataset,use.pred=F,omit.possible=T,...)
# {
# 	library(e1071)
# 	pairs=dataset$pairs[,-c(1:2)]
# 	pairs[is.na(pairs)]=0
# 	if (use.pred)
#     {
#     	if (is.null(dataset$prediction))
#     		stop ("No prediction vector in dataset! Call with use.pred=T.")
# 		pairs$is_match=dataset$prediction
# 	} else
# 	{
# 		pairs$is_match=factor(pairs$is_match)
# 		levels(pairs$is_match)=c("N","L","P")
# 		pairs$is_match[is.na(pairs$is_match)]="P"		
# 	}
# 	# delete possible links, not suitable for training
# 	if (omit.possible)
# 		pairs=pairs[pairs$is_match!="P",]
# 
#     model=svm(is_match ~ .,data=pairs,type="C-classification",...)
# 
# 	ret=list(train=dataset,model=model)
# 	class(ret)="RecLinkClassif"
#     return(ret)
# }
# 
# classify.rpart = function(model,newdata,...)
# {
# 	library(e1071)
# 	ret=newdata
# 	if (ncol(model$train$pairs)!=ncol(newdata$pairs))
# 		stop ("Mismatching number of attributes!")
# 
#     x=newdata$pairs[,-c(1,2,ncol(newdata$pairs))]
#     x[is.na(x)]=0
# 
#     predict=predict(model$model, newdata=x,type="class")       
#     ret$prediction=predict
#     class(ret)="RecLinkResult"
#     return(ret)
# 
# }
# 
# classify.ada = function(model,newdata,...)
# {
# 	library(e1071)
# 	ret=newdata
# 	if (ncol(model$train$pairs)!=ncol(newdata$pairs))
# 		stop ("Mismatching number of attributes!")
# 
#     x=newdata$pairs[,-c(1,2,ncol(newdata$pairs))]
#     x[is.na(x)]=0
# 
#     predict=predict(model$model, newdata=x,type="class")       
#     ret$prediction=predict
#     class(ret)="RecLinkResult"
#     return(ret)
# 
# }
