# Epilink mit Kreuzvalidierung, mit echten Validierungsdaten

# Eingabe: 
#   - Trainingsdaten (generiert)
#   - Validierungsdaten (echt)

classify.svm = function (train, valid)
{
    library(e1071)
    train$pairs[is.na(train$pairs)]=0
    valid$pairs[is.na(valid$pairs)]=0
    if (is.null(train$is_match))
    {
        is_match_train=train$identity[train$pairs$id1]==train$identity[train$pairs$id2]
    } else is_match_train=train$is_match
    if (is.null(valid$is_match))
    {    
        is_match_valid=valid$identity[valid$pairs$id1]==valid$identity[valid$pairs$id2]
    } else is_match_valid=valid$is_match    
    x=as.matrix(train$pairs[,-(1:2)])
    y=as.matrix(as.integer(is_match_train))
    model=svm(y ~ x, kernel="radial",type="C-classification")
    x=as.matrix(valid$pairs[,-(1:2)])
    predict=predict(model, newdata=x)       
    valid$prediction=predict
    return(valid)
}

