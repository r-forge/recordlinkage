# Epilink mit Kreuzvalidierung, mit echten Validierungsdaten

# Eingabe: 
#   - Trainingsdaten (generiert)
#   - Validierungsdaten (echt)
#   - Fehlerrate
#   - Anz. Kreuzvalidierungen

classify.epilink = function (rpairs, threshold=NULL,...)
{
    ret=rpairs
    n_attr=ncol(rpairs$train)
    rpairs$valid[is.na(rpairs$valid)]=0
    f=rpairs$frequencies

    if (missing(threshold))
    { 
        rpairs$train[is.na(rpairs$train)]=0
        weights_train=epiWeights(rpairs$train[,-c(1,2,n_attr)], rpairs$train$is_match, f)
        weights_train=weights_train[order(weights_train$Weight, decreasing=TRUE),]
    	threshold=getThreshold(weights_train$Weight,weights_train$is_match)
    } else if (!is.numeric(threshold))
        stop("threshold must be numeric")
    weights_valid=epiWeights(rpairs$valid[,-c(1,2,n_attr)], rpairs$valid$is_match, f)
    ret$prediction=weights_valid$Weight>=threshold
    ret$model=threshold
    class(ret)="RecLinkResult"
    return(ret)
}



# Eingabe: Daten und Matchingstatus
# Ausgabe: Daten, Matchingstatus, Gewicht
epiWeights <- function(data, is_match, f)
{
 # Fehlerrate festlegen
 e=0.01
 # durchschnittliche Häufigkeiten
 
 # aus Datenbank berechnet
 # aname bleibt draußen
 w=log((1-e)/f, base=2)
 # Gewichtsberechnung
 row_sum <- function(r,w)
 {
  return(sum(as.numeric(r)*w))
 }
# S=apply(data,1,row_sum,w)/sum(w)
 S=colSums(w*t(data))/sum(w)
# o=order(S, decreasing=TRUE)
# ret_mat=cbind(data[o,],is_match[o], S[o])
 ret_mat=cbind(data,is_match, S)
 colnames(ret_mat)[ncol(ret_mat)]="Weight"
 colnames(ret_mat)[ncol(ret_mat)-1]="is_match"
 return(as.data.frame(ret_mat))
}

# war: getThresholdMinAll
getThreshold <- function (weights, is_match)
{
	n_data=length(is_match)
	if (n_data!=length(weights))
	{
		stop("Argumente weights und is_match haben unterschiedliche Länge")
	}
	# Zwei Fehlerraten werden gebildet: der erste die Fehlerrate für falsch
	# positive, die zweite für falsch negative; für jedes Datenpaar werden die
	# Fehler berechnet, die entstehen, wenn an diesem Datenpaar der Threshold
	# gesetzt wird. Die Summe der beiden Fehler ergibt den zu minimierenden
	# Gesamtfehler.
	fehlerrate_1=cumsum(is_match!=1)/as.numeric(1:n_data)
    fehlerrate_2=rev(cumsum(rev(is_match==1)/as.numeric(1:n_data)))
    fehlerrate=fehlerrate_1+fehlerrate_2

    # nun baue Tabelle, in der Gewicht (unique) und Fehlerrate gegenübergestellt
    # sind. Die Fehlerrate eines Gewichts ist in der sortierten Tabelle gleich
    # der Fehlerrate für den letzten Datensatz des Blocks
    # tapply() sortiert aufsteigend, das rev() stellt 
    # die absteigende Reihenfolge wieder her

    fehlerrate_unique=rev(tapply(fehlerrate,weights,tail,1))
    weights_unique=unique(weights)

    # Bestimme Gewicht des Datensatzes mit minimalem Fehler.
    threshold=as.numeric(weights_unique[which.min(fehlerrate_unique)])
	return (threshold)
}

