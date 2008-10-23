# Epilink mit Kreuzvalidierung, mit echten Validierungsdaten

# Eingabe: 
#   Daten
#   method: rpart, bagging, boosting


classify.rpart = function (rpairs,...)
{
    library(rpart)
    library(ada)
    library(ipred)
    
    if (nrow(rpairs$train)==0)
        stop("No training set in rpairs!")
        
    train=rpairs$train[,-c(1,2)]
    train$is_match=factor(train$is_match)
    valid=rpairs$valid[,-c(1,2)]
    model=rpart(is_match ~ ., data=train,method="class",...)
    pred=predict(model, newdata=valid)
    ret=rpairs
#    ret$prediction=pred
    ret$prediction=pred[,"TRUE"]>pred[,"FALSE"]
    return(ret)
}

classify.bagging = function (rpairs,...)
{
    library(ipred)
    
    if (nrow(rpairs$train)==0)
        stop("No training set in rpairs!")
        
    # the columns of train and valid have to be coerced to factors with
    # same levels, NA being a factor
    train=rpairs$train[,-c(1,2)]
    valid=rpairs$valid[,-c(1,2)]
    # for bagging, all data has to be converted to factors with equal levels
    # for train and valid, NA being a level
    levels=mapply(function(x,y) union(x,y),train,valid)
    train=as.data.frame(mapply(function(x,y) list(factor(x,levels=y,exclude=NULL)),train,levels))
    valid=as.data.frame(mapply(function(x,y) list(factor(x,levels=y,exclude=NULL)),valid,levels))
    model=bagging(is_match ~ ., data=train,method="class",...)
    pred=predict(model, newdata=valid)
    ret=rpairs
    ret$prediction=pred
    return(ret)
}

classify.ada = function (rpairs,...)
{
    library(ada)
    
    if (nrow(rpairs$train)==0)
        stop("No training set in rpairs!")
        
    train=rpairs$train[,-c(1,2)]
    train$is_match=factor(train$is_match)
    valid=rpairs$valid[,-c(1,2)]
    # for ada, all data has to be converted to factors with equal levels
    # for train and valid, NA being a level
    levels=mapply(function(x,y) union(x,y),train,valid)
    train=as.data.frame(mapply(function(x,y) list(factor(x,levels=y,exclude=NULL)),train,levels))
    valid=as.data.frame(mapply(function(x,y) list(factor(x,levels=y,exclude=NULL)),valid,levels))
    model=ada(is_match ~ ., data=train,...)
    pred=predict(model, newdata=valid)
    ret=rpairs
    ret$prediction=pred
    return(ret)
}
