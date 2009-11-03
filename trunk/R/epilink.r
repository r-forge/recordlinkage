# epilink.r: Functions for the Epilink matching procedure
# See Continiero et al.: The EpiLink record linkage software, in:
# Methods of Information in Medicine 2005, 44(1):66-71.


epiWeights <- function (rpairs, e=0.01, f=rpairs$frequencies)
{

 # leave out ids and matching status
 pairs=rpairs$pairs[,-c(1,2,ncol(rpairs$pairs))]
 pairs[is.na(pairs)]=0
 # error rate
 w=log((1-e)/f, base=2)

 # weight computation
 row_sum <- function(r,w)
 {
  return(sum(r*w))
 }

 S=apply(pairs,1,row_sum,w)/sum(w)
 rpairs$Wdata=S
 return(rpairs)
}







# Arguments:
#   rpairs      weighted record pairs (output of emWeights)
#   my      error bound: # False Matches / # Found Matches
#   ny      error bound  # False Non-Matches / # Found Non-Matches
#       If an error bound is Inf, it will not be considered, meaning that
#       "possible link" will not be assigned
epiClassify <- function (rpairs,threshold.upper, 
                        threshold.lower=threshold.upper)
{    
#    o=order(rpairs$W,decreasing=TRUE) # order Weights decreasing
#
#    # if no threshold was given, compute them according to the error bounds
#    if (missing(threshold.upper) && missing(threshold.lower))
#    {
#      FN=rev(cumsum(rev(rpairs$M[o]))) 
#      FP=cumsum(rpairs$U[o])
#      if (my==Inf && ny==Inf)
#      {
#          # no error bound given: minimize overall error
#          cutoff_upper=which.min(c(0,FP)+c(FN,0))-1
#          if (length(cutoff_upper)==0)
#              cutoff_upper=0
#          cutoff_lower=cutoff_upper
#          
#      } else if (my==Inf)
#      {  
#          # only rate of false matches relevant
#          cutoff_lower=head(which(FN<=ny),1)
#          if (length(cutoff_lower)==0)
#              cutoff_lower=length(o)
#          cutoff_upper=cutoff_lower
#      
#      } else if (ny==Inf)
#      {
#          # only rate of false non-matches relevant
#          cutoff_upper=tail(which(FP<=my),1)
#          cutoff_lower=cutoff_upper
#      } else
#      {
#          # both error bounds relevant
#          cutoff_upper=tail(which(FP<=my),1)
#          cutoff_lower=head(which(FN<=ny),1)
#          if (length(cutoff_upper)==0)
#              cutoff_upper=0
#          if (length(cutoff_lower)==0)
#              cutoff_lower=length(o)
#          if (cutoff_lower<cutoff_upper)
#          {
#              cutoff_upper=which.min(c(0,FP)+c(FN,0))-1
#              cutoff_lower=cutoff_upper
#          }
#      } 
#      print("Threshold berechnen und Klassifikation zuweisen")
#      threshold.upper=rpairs$W[o][cutoff_upper]
#      threshold.lower=rpairs$W[o][cutoff_lower]
#    } # end if
#     
    prediction=rep("P",nrow(rpairs$pairs))
    prediction[rpairs$Wdata>=threshold.upper]="L"
    prediction[rpairs$Wdata<threshold.lower]="N"
    
    ret=rpairs # keeps all components of rpairs
    ret$prediction=factor(prediction,levels=c("N","P","L"))
  	ret$threshold=threshold.upper
    class(ret)="RecLinkResult"
    return(ret)
}

# EM mit externen Trainingsdaten ist erst mal auf Eis gelegt!

# supervised learning with EM
# emClassify2 <- function (rpairs, my=Inf, ny=Inf)
# {    
#     train=rpairs$train
#     train=train[,-c(1,2,ncol(train))] # delete ids and is_match
#     train[is.na(train)]=0 # convert NAs to 0
#     #o=order(rpairs$W,decreasing=T) # order Weights decreasing
#     n_attr=ncol(train) # number of attributes
#     # For each record pair, compute index of corresponding pattern in the
#     # table of binary combinations
#     indices=colSums(t(train)*(2^(n_attr:1-1)))+1    
#     train_weights=rpairs$W[indices]
# 
#     valid=rpairs$valid
#     valid=valid[,-c(1,2,ncol(valid))] # delete ids and is_match
#     valid[is.na(valid)]=0 # convert NAs to 0
#     #o=order(rpairs$W,decreasing=T) # order Weights decreasing
#     n_attr=ncol(valid) # number of attributes
#     # For each record pair, compute index of corresponding pattern in the
#     # table of binary combinations
#     indices=colSums(t(valid)*(2^(n_attr:1-1)))+1    
#     valid_weights=rpairs$W[indices]
# 
#     o=order(train_weights,decreasing=T)
#     threshold=getThresholdMinAll(train_weights[o],rpairs$train$is_match[o])
#     prediction=valid_weights>=threshold
# #     
# #     L_ind=o[1:cutoff_upper] # indices of detected links
# #     U_ind=o[(cutoff_lower+1):length(o)] # indices of detected non-links
# #     # links get result TRUE, non-links FALSE, possible links NA
# #     prediction=as.logical(rep(NA,nrow(pairs)))
# #     prediction[which(is.element(indices,L_ind))]=T
# #     prediction[which(is.element(indices,U_ind))]=F
# 
#     ret=rpairs # keeps all components of rpairs
#     ret$prediction=prediction
#     class(ret)="RecLinkResult"
#     return(ret)
# }
# 
# getThreshold <- function (weights, is_match, max_fehler=0.01)
# {
#   #return (getThresholdMaxFP(weights, is_match, max_fehler))
#   return (getThresholdMinAll(weights, is_match))
# }
# 
# getThresholdMinAll <- function (weights, is_match)
# {
# 	n_data=length(is_match)
# 	if (n_data!=length(weights))
# 	{
# 		stop("Argumente weights und is_match haben unterschiedliche Länge")
# 	}
# 	# Zwei Fehlerraten werden gebildet: der erste die Fehlerrate für falsch
# 	# positive, die zweite für falsch negative; für jedes Datenpaar werden die
# 	# Fehler berechnet, die entstehen, wenn an diesem Datenpaar der Threshold
# 	# gesetzt wird. Die Summe der beiden Fehler ergibt den zu minimierenden
# 	# Gesamtfehler.
#         # no error bound given: minimize overall error
#         
#     fehlerrate_1=cumsum(is_match!=1)/as.numeric(1:n_data) # false positive
#     fehlerrate_2=rev(cumsum(rev(is_match==1)/as.numeric(1:n_data))) # false negative
#     fehlerrate=c(0,fehlerrate_1)+c(fehlerrate_2,0)
#     return(weights[which.min(fehlerrate)])
# 
#     # nun baue Tabelle, in der Gewicht (unique) und Fehlerrate gegenübergestellt
#     # sind. Die Fehlerrate eines Gewichts ist in der sortierten Tabelle gleich
#     # der Fehlerrate für den letzten Datensatz des Blocks
#     # tapply() sortiert aufsteigend, das rev() stellt 
#     # die absteigende Reihenfolge wieder her
# 
#     fehlerrate_unique=rev(tapply(fehlerrate,c(weights,-Inf),tail,1))
# #     weights_unique=unique(weights)
# # 
# #     # Bestimme Gewicht des Datensatzes mit minimalem Fehler.
# #     threshold=as.numeric(weights_unique[which.min(fehlerrate_unique)])
# # 	return (threshold)
# }
# 
# getThresholdMaxFP <- function (weights, is_match, max_fehler=0.01)
# {
# 	n_data=length(is_match)
# 	if (n_data!=length(weights))
# 	{
# 		stop("Argumente weights und is_match haben unterschiedliche Länge")
# 	}
#     fehlerrate=cumsum(is_match==0)/as.numeric(1:n_data)
#     # nehme den minimalen Threshold, für den die Fehlerschranke noch eingehalten
#     # wird
#     threshold=as.numeric(tail(weights[fehlerrate<max_fehler],1))
#     # Abfangen des Falls, dass nur Nicht-Matche in Stichprobe sind: setze
#     # Threshold auf Maximum der Gewichte
#     if(identical(threshold,numeric(0)))
#     {
#      threshold=max(weights)
#     }
# 	return (threshold)
# }
# 
