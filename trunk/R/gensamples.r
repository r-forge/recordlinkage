# function gensamples 
#
# generates training data pairs from data items with no specified class
# membership by means of nearest neighbor clustering, fellegi-sunter modelling
# on the base of function emgllm or deterministic clustering
#
# Requires packages e1071 and mygllm
#
# parameter:
#
#   datapairs:      data pairs as input without known duplicate status, from compare.r
#
#   des_prop:       desired proportion of duplicates to non-duplicates
# 
#   num_non:        Number of desired non-matches of data pairs 
#
#   rseed:          random seed
#
#   cler_review:    Number of data pairs for clerical review => NO  
#
#   ids:            2-column identificator matrix of data pairs => NO
#
#   undet:          consideration of undetermined cases => NO
#   
#   model_sel:      nn clustering, em or deterministic clustering => NO

library(e1071)
library(mygllm)
gensamples = function (datapairs, num_non, des_prop=0.1, nclasses=2, seed=10)
{   
    set.seed(seed)
    ngesamt=0
    ret=datapairs   
    dmat=as.matrix(ret$pairs) # any filters will be set
    ids=dmat[,c(1:2)]
    # delete ids if existing; it exists, so delete the if sequence
    dpairs=dmat[,-c(1:2)] 
    #  number of classes is 3, when undetermindes cases are allowed
    # if (undet) nclasses=nclasses+1
    #  consistence checking
    nlink=des_prop*(num_non) #+cler_review)
    ngesamt=num_non+nlink # +cler_review
    ndata=nrow(ids)
    if (ngesamt > ndata){stop("Inconsistent values for training data!")}
    # run clustering algorithm, Choosing?
    # res_clust <- bclust(x=dmat, centers=3,base.center=2, base.method="kmeans", iter.base=100, verbose=T)
    # kmendundet<-kmeans(x=dmat, centers=nclasses)
    # pairs$pairs[kclusr$cluster==2,]
    # Use EM algorithm instead    
    ddpairs<-emWeights(ret)
    ddclass<-emClassify(ddpairs)
    linksid<-ids[ddclass$prediction,]
    nonlinksid<-ids[!ddclass$prediction,]
    alidn<-nrow(linksid)
    anolidn<-nrow(nonlinksid)
    # sample is fast enough for not including it in the if-clauses
    if(nlink > alidn) { warning("Only ", alidn, " Links!"); nlink=alidn }
    if(num_non > anolidn) { warning("Only ", anolidn, " Non-Links!"); num_non=anolidn }
    # Assumption: only two classes, then draw samples

# kann man nicht auch mit Zeilennummern der pairs arbeiten? spart merge!
    salid<-sample(x=1:alidn, size=nlink)
    sanolid<-sample(x=1:anolidn, size=num_non)
    slinksid=merge(salid,cbind(1:alidn,linksid),by.x=1,by.y=1,sort=F)[,-1]
    snonlinksid=merge(sanolid,cbind(1:anolidn,nonlinksid),by.x=1,by.y=1,sort=F)[,-1]
    trainallid=rbind(slinksid, snonlinksid)
    help1=paste(trainallid[,1],trainallid[,2])
    help2=paste(ids[,1],ids[,2])
    trainhelp=help2 %in% help1 # T, falls Paar in Trainingsmenge ist
    comptrainhelp=!trainhelp   # Komplement
    help3=paste(slinksid[,1],slinksid[,2])
    linkhelp=help2 %in% help3  # T, falls Paar Links ist und in Trainingsmenge
    help4=paste(snonlinksid[,1],snonlinksid[,2])
    nonlinkhelp=help2 %in% help4 # T, falls Paar Non-Link ist und in Trainingsmenge
    #ret$evals=ret$pairs[trainhelp,]
    ret$evalids=comptrainhelp
    ret$linkids=linkhelp
    ret$nonlinkids=nonlinkhelp
    #slinks=merge(slinksid, ret$pairs, by.x=c(1,2), by.y=c(1,2),sort=F)
    #snonlinks=merge(snonlinksid, ret$pairs, by.x=c(1,2), by.y=c(1,2),sort=F)
    # trainall=merge(trainallid, ret$pairs, by.x=c(1,2), by.y=c(1,2),sort=F)
    #ret$slinks=slinks
    #ret$snonlinks=snonlinks
    class(ret)="RecLinkPairs"
    return(ret)
}


    #gensamples(pairs, 100000)
  
    # left=merge(pairs$pairs[kclusr$cluster==2,,],cbind(1:nrow(data),data),by.x=1,by.y=1,sort=F)
    # right=merge(pairs$pairs[kclusr$cluster==2,,],cbind(1:nrow(data),data),by.x=2,by.y=1,sort=F)
    # => bad results of clustering, so use mygllm instead and this implies:
    # em is used directly for RL or samples are generated for other methods  
