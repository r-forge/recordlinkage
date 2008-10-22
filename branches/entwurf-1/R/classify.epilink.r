# Epilink mit Kreuzvalidierung, mit echten Validierungsdaten

# Eingabe: 
#   - Trainingsdaten (generiert)
#   - Validierungsdaten (echt)
#   - Fehlerrate
#   - Anz. Kreuzvalidierungen

classify.epilink = function (train, valid)
{
    train$pairs[is.na(train$pairs)]=0
    valid$pairs[is.na(valid$pairs)]=0
    if (is.null(train$is_match))
    {
        is_match_train=train$identity[train$pairs$id1]==train$identity[train$pairs$id2]
    } else is_match_train=train$is_match
    weights_train=epiWeights(train$pairs, is_match_train, train$frequencies)
    weights_train=weights_train[order(weights_train$Weight, decreasing=TRUE),]
    if (is.null(valid$is_match))
    {    
        is_match_valid=valid$identity[valid$pairs$id1]==valid$identity[valid$pairs$id2]
    } else is_match_valid=valid$is_match
#    weights_valid=epiWeights(valid$pairs, is_match_valid, train$frequencies)
#    weights_valid=epiWeights(valid$pairs, is_match_valid, valid$frequencies)
    f=c( 0.000186,0.000843, 0.0000376, 0.00232,0.033,0.083,0.014)
    weights_valid=epiWeights(valid$pairs, is_match_valid, f)
    weights_valid=weights_valid[order(weights_valid$Weight, decreasing=TRUE),]
	threshold=getThreshold(weights_train$Weight,weights_train$is_match)
    table=table(is_match_valid,weights_valid$Weight>threshold)
    print(table)
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
 S=apply(data[,-(1:2)],1,row_sum,w)/sum(w)
 o=order(S, decreasing=TRUE)
 ret_mat=cbind(data[o,],is_match[o], S[o])
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

