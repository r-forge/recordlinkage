# function genSamples 
#
# generates training data pairs from data items with no specified class
# membership by means of nearest neighbor clustering, fellegi-sunter modelling
# on the base of function emgllm or deterministic clustering
#
# Requires package e1071
#
# parameter:
#
#   datapairs:      data pairs as input without known duplicate status, from compare.r
#
#   des.prop:       desired proportion of duplicates to non-duplicates
# 
#   num.non:        Number of desired non-matches of data pairs 
#
#   rseed:          random seed
#
#   adjust:         Boolean for adjusting numbers for Links, Nonlinks to parameters
#
#   cler_review:    Number of data pairs for clerical review => NO  
#
#   ids:            2-column identificator matrix of data pairs => NO
#
#   undet:          consideration of undetermined cases => NO
#   
#   model_sel:      nn clustering, em or deterministic clustering => NO

library(e1071)
genSamples = function (datapairs, num.non, des.prop=0.1, seed=10, adjust=F)
{   
    set.seed(seed)
    ret <- datapairs
    if (nrow(datapairs$train)!=0)
        stop("Training set already initialized!")
    pairs=ret$valid
    # delete existing is_match column
    pairs$is_match=NULL
    ndata <- nrow(pairs)
    #ids=as.matrix(ret$pairs[,c(1:2)]) # any filters will be set
    ids <- seq(from=1,to=ndata)
    #  number of classes is 3, when undetermindes cases are allowed
    # if (undet) nclasses=nclasses+1
    #  consistence checking
    nlink <- round(des.prop*(num.non)) #+cler_review)
    ngesamt <- num.non+nlink # +cler_review
    if (ngesamt > ndata){stop("Inconsistent values for training data!")}
    if (des.prop<0 || des.prop >=1){stop("Inconsistent value for link proportion!")}
    # run clustering algorithm?
    # res_clust <- bclust(x=ddmat, centers=3,base.center=2, base.method="kmeans", iter.base=100, verbose=T)
    # kmendundet<-kmeans(x=ddmat, centers=nclasses); pairs$pairs[kclusr$cluster==2,]
    # Use EM algorithm instead    
    ddpairs <- emWeights(ret)
    ddclass <- emClassify(ddpairs)
    # linksid<-ids[ddclass$prediction,]
    # nonlinksid<-ids[!ddclass$prediction,]
    linksid <- ids[ddclass$prediction]
    nonlinksid <- ids[!ddclass$prediction]
    
    # alidn<-nrow(linksid)
    # anolidn<-nrow(nonlinksid)
    alidn <- length(linksid)
    anolidn <- length(nonlinksid)

    nmark=F; mmark=F;
    # sample is fast enough for not including it in the if-clauses
    if(nlink > alidn) { warning("Only ", alidn, " Links!"); nlink=alidn; nmark=T }
    if(num.non > anolidn) { warning("Only ", anolidn, " Non-Links!"); num.non=anolidn; mmark=T }
    if (adjust==T && nmark==T ) {num.non=round(nlink/des.prop)}
    if (adjust==T && mmark==T ) {nlink=round(des.prop*num.non)}
    # Assumption: only two classes, then draw samples
#     print(linksid)
#     print(nonlinksid)
    salid <- resample(linksid, size=nlink)
    sanolid <- resample(nonlinksid, size=num.non)
#     print(salid)
#     print(sanolid)
    # salid <- sample(x=1:alidn, size=nlink)
    # sanolid <- sample(x=1:anolidn, size=num.non)
    
    trainid <- c(salid,sanolid)
    trainhelp <- ids %in% trainid
    linkhelp <- ids %in% salid
    nonlinkhelp <- ids %in% sanolid
    ret$valid <- cbind(pairs[!trainhelp,],is_match=datapairs$valid$is_match[!trainhelp])
    ret$train <- rbind(cbind(pairs[linkhelp,],is_match=ddclass$prediction[linkhelp]),
                       cbind(pairs[nonlinkhelp,],is_match=ddclass$prediction[nonlinkhelp]))
#     ret$slinks <- ret$pairs[linkhelp,]
#     ret$snonlinks <- ret$pairs[nonlinkhelp,]
    
    class(ret)="RecLinkData"
    return(ret)
}
    
    # Use two id's   
#   slinksid=merge(salid,cbind(1:alidn,linksid),by.x=1,by.y=1,sort=F)[,-1]
#   snonlinksid=merge(sanolid,cbind(1:anolidn,nonlinksid),by.x=1,by.y=1,sort=F)[,-1]
#   trainallid=rbind(slinksid, snonlinksid)
#   help1=paste(trainallid[,1],trainallid[,2])
#   help2=paste(ids[,1],ids[,2])
#   trainhelp=help2 %in% help1
#   comptrainhelp=!trainhelp
#   slinks=merge(slinksid, ret$pairs, by.x=c(1,2), by.y=c(1,2),sort=F)
#   snonlinks=merge(snonlinksid, ret$pairs, by.x=c(1,2), by.y=c(1,2),sort=F)
#   trainall=merge(trainallid, ret$pairs, by.x=c(1,2), by.y=c(1,2),sort=F)
#   ret$slinks=slinks
#   ret$snonlinks=snonlinks
#   ret$evals=ret$pairs[comptrainhelp,]
#   class(ret)="RecLinkPairs"
#   return(ret)

#   only ID's within the 2-id's solution => outperformed
#   help3=paste(slinksid[,1],slinksid[,2])
#   linkhelp=help2 %in% help3
#   help4=paste(snonlinksid[,1],snonlinksid[,2])
#   nonlinkhelp=help2 %in% help4
#   ret$evals=ret$pairs[trainhelp,]
#   ret$evalids=comptrainhelp
#   ret$linkids=linkhelp
#   ret$nonlinkids=nonlinkhelp
#   Usage: gensamples(pairs, 100000)

    
  
    # => bad results of clustering, so use mygllm instead and this implies:
    # em is used directly for RL or samples are generated for other methods  