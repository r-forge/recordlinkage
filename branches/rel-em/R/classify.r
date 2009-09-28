# classify.r: Functions for Record Linkage with machine learning algorithms.

trainSupv <- function(dataset,method,use.pred=FALSE,omit.possible=TRUE,
                convert.na=TRUE, include.data=FALSE, ...)
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
  ret=list()
 	if (isTRUE(include.data))
	   ret$train=dataset
  ret$model=model
  ret$method=method
	class(ret)="RecLinkClassif"
    return(ret)

}


classifySupv <- function(model,newdata,...)
{
	ret=newdata

    x=newdata$pairs[,-c(1,2,ncol(newdata$pairs))]
	x[is.na(x)]=0

    predict=switch(model$method,
  		svm=predict(model$model, newdata=x,...),       
	 	 rpart=predict(model$model, newdata=x,type="class",...),       
		  ada=predict(model$model, newdata=x,type="vector",...),       
		  bagging=predict(model$model, newdata=x,type="class",...),
		  nnet=predict(model$model, newdata=x,type="class",...))
    # refactor to ensure uniform order of levels
    ret$prediction=factor(predict,levels=c("N","P","L"))
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
    # refactor to ensure uniform order of levels
    dataset$prediction=factor(dataset$prediction,levels=c("N","P","L"))
		class(dataset)="RecLinkResult"
		return(dataset)	
	}
	stop("Illegal method!")
}


