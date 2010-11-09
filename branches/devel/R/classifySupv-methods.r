# Methods for s$ generic classifyUnsupv


setGeneric(
  name= "classifySupv",
  def=function(x, y, ...){standardGeneric("classifySupv")}
)

#' Method for old (S3-style) objects, replaces ordinary function
setMethod(
  f = "classifySupv",
  signature = c("RecLinkClassif", "RecLinkData"),
  definition = function (x, y, ...)
  {
    # redefine to improve readability
    model <- x
    newdata <- y

    # type checks from previous version omitted, now enforced by 
    # method dispatching  
        
  	ret=newdata
  
      x=newdata$pairs[,-c(1,2,ncol(newdata$pairs))]
  	x[is.na(x)]=0
  
      predict=switch(model$method,
    		svm=predict(model$model, newdata=x,...),       
  	 	 rpart=predict(model$model, newdata=x,type="class",...),       
  		  ada=predict(model$model, newdata=x,type="vector",...),       
  		  bagging=predict(model$model, newdata=x,type="class",...),
  		  nnet=predict(model$model, newdata=x,type="class",...),
        stop("Illegal classification method!"))
      # refactor to ensure uniform order of levels
      ret$prediction=factor(predict,levels=c("N","P","L"))
      class(ret)="RecLinkResult"
      return(ret)
  }
)

# Methods for big data sets, first two arguments can be switched
setMethod(
  f = "classifySupv",
  signature = c("RLBigData", "RecLinkClassif"),
  definition = function(x, y, ...) classifySupvBigData(rpairs = x, model = y, ...)
)
  
setMethod(
  f = "classifySupv",
  signature = c("RecLinkClassif", "RLBigData"),
  definition = function(x, y, ...) classifySupvBigData(rpairs = y, model = x, ...)
)

#' Internal workhorse function
classifySupvBigData <- function(rpairs, model, stepsize=10000, ...)
{
 on.exit(clear(rpairs))
 rpairs <- begin(rpairs)
 links <- matrix(0L, 0L, nrow=0, ncol=2)
 i = stepsize
 while(nrow(slice <- nextPairs(rpairs,stepsize)) > 0)
 {
  message(i)
  flush.console()
  # vorläufig!
#   slice <- data.frame(as.integer(slice[,1]),
#   as.integer(slice[,2]), as.matrix(slice[,3:ncol(slice)]))
   colnames(slice) <- c("id1", "id2", levels(model$model$frame$var)[-1])
    prediction=switch(model$method,
  	  svm=predict(model$model, newdata=slice,...),       
        rpart=predict(model$model, newdata=slice,type="class",...),       
  		  ada=predict(model$model, newdata=slice,type="vector",...),       
  		  bagging=predict(model$model, newdata=slice,type="class",...),
  		  nnet=predict(model$model, newdata=slice,type="class",...),
        stop("Illegal classification method!"))
   links <- rbind(links, slice[prediction=="L",1:2]) 
   i <- i + stepsize
 }      
 links
}

