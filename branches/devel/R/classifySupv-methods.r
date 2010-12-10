# Methods for s$ generic classifyUnsupv


setGeneric(
  name= "classifySupv",
  def=function(model, newdata, ...){standardGeneric("classifySupv")}
)

#' Method for old (S3-style) objects, replaces ordinary function
setMethod(
  f = "classifySupv",
  signature = c("RecLinkClassif", "RecLinkData"),
  definition = function (model, newdata, ...)
  {

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

# Methods for big data sets
setMethod(
  f = "classifySupv",
  signature = c("RecLinkClassif", "RLBigData"),
  definition = function(model, newdata, ...) classifySupvBigData(rpairs = newdata, model = model, ...)
)

# First two arguments can be switched
setMethod(
  f = "classifySupv",
  signature = c("RLBigData", "RecLinkClassif"),
  definition = function(model, newdata, ...) classifySupvBigData(rpairs = model, model = newdata, ...)
)
  

#' Internal workhorse function
classifySupvBigData <- function(rpairs, model, stepsize=10000, ...)
{
  on.exit(clear(rpairs))
  rpairs <- begin(rpairs)
  links <- matrix(0L, 0L, nrow=0, ncol=2)
  possibleLinks <- matrix(0L, 0L, nrow=0, ncol=2)
  nPairs <- 0
  while(nrow(slice <- nextPairs(rpairs,stepsize)) > 0)
  {
   # Spaltennamen angleichen
   # TODO: Fehlerbehandlung für ungleiche Attributanzahl
   colnames(slice) <- c("id1", "id2", levels(model$model$frame$var)[-1])
    prediction=switch(model$method,
  	  svm=predict(model$model, newdata=slice,...),       
        rpart=predict(model$model, newdata=slice,type="class",...),       
  		  ada=predict(model$model, newdata=slice,type="vector",...),       
  		  bagging=predict(model$model, newdata=slice,type="class",...),
  		  nnet=predict(model$model, newdata=slice,type="class",...),
        stop("Illegal classification method!"))
   links <- rbind(links, as.matrix(slice[prediction=="L",1:2]))
   possibleLinks <- rbind(possibleLinks, as.matrix(slice[prediction=="P",1:2]))
   nPairs <- nPairs + nrow(slice)
   message(nPairs)
   flush.console()
  }      
  result <- new("RLResult", data = rpairs, links = links, 
    possibleLinks = possibleLinks, nPairs = nPairs)
}

