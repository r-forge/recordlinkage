
setGeneric(
  name = "epiClassify",
  def = function(rpairs, threshold.upper, threshold.lower=threshold.upper, ...)
    standardGeneric("epiClassify")
)

setMethod(
  f = "epiClassify",
  signature = "RecLinkData",
  definition = function (rpairs,threshold.upper, 
                        threshold.lower=threshold.upper)
  {    
  
    if (!("RecLinkData" %in% class(rpairs) || "RecLinkResult" %in% class(rpairs)))
      stop(sprintf("Wrong class for rpairs: %s", class(rpairs)))
  
    if (nrow(rpairs$pairs) == 0)
      stop("No record pairs!")
  
    if (is.null(rpairs$Wdata))
      stop("No weights in rpairs!")
  
    if (!is.numeric(threshold.upper))
      stop(sprintf("Illegal type for threshold.upper: %s", class(threshold.upper)))
  
    if (!is.numeric(threshold.lower))
      stop(sprintf("Illegal type for threshold.lower: %s", class(threshold.lower)))
  
    if (threshold.upper < threshold.lower)
      stop(sprintf("Upper threshold %g lower than lower threshold %g",
      threshold.upper, threshold.lower))
      
    prediction=rep("P",nrow(rpairs$pairs))
    prediction[rpairs$Wdata>=threshold.upper]="L"
    prediction[rpairs$Wdata<threshold.lower]="N"
    
    ret=rpairs # keeps all components of rpairs
    ret$prediction=factor(prediction,levels=c("N","P","L"))
    ret$threshold=threshold.upper
    class(ret)="RecLinkResult"
    return(ret)
  }
) # end of setMethod

setMethod(
  f = "epiClassify",
  signature = "RLBigData",
  definition = function (rpairs,threshold.upper, 
                        threshold.lower=threshold.upper, e=0.01, 
                        f=getFrequencies(rpairs))
  {    
    if (!is.numeric(threshold.upper))
      stop(sprintf("Illegal type for threshold.upper: %s", class(threshold.upper)))
  
    if (!is.numeric(threshold.lower))
      stop(sprintf("Illegal type for threshold.lower: %s", class(threshold.lower)))
  
    if (threshold.upper < threshold.lower)
      stop(sprintf("Upper threshold %g lower than lower threshold %g",
      threshold.upper, threshold.lower))

    if (dbExistsTable(rpairs@con, "Wdata"))
    {
      query <- "select id1, id2 from Wdata where W >= :upper"
      links <- dbGetPreparedQuery(rpairs@con, query, data.frame(upper = threshold.upper))
      query <- "select id1, id2 from Wdata where W < :upper and W >= :lower"
      possibleLinks <- dbGetPreparedQuery(rpairs@con, query,
        data.frame(upper = threshold.upper, lower = threshold.lower))
      nPairs <- dbGetQuery(rpairs@con, "select count(*) as c from Wdata")$c
    } else
    {

      on.exit(clear(rpairs))
      rpairs <- begin(rpairs)
      nPairs <- 0
      n <- 10000
      i = n
      links <- matrix(nrow=0, ncol=2)
      possibleLinks <- matrix(nrow=0, ncol=2)
      while(nrow(slice <- nextPairs(rpairs, n)) > 0)
      {
  #      message(i)
        flush.console()
        slice[is.na(slice)] <- 0
        e=e+rep(0,ncol(slice)-3)
        f=f+rep(0,ncol(slice)-3)
        # adjust error rate
        # error rate
        w=log((1-e)/f, base=2)
        #


        # weight computation
        row_sum <- function(r,w)
        {
          return(sum(r*w,na.rm=TRUE))
        }
        sumW <- sum(w)
        S=apply(slice[,-c(1,2,ncol(slice))],1,row_sum,w)/sumW
        if (any(is.na(S) | S < 0 | S > 1))
          warning("Some weights have illegal values. Check error rate and frequencies!")
  #      message(range(slice[,1]))
  #      message(range(slice[,2]))
  #      message("----------------------")
        links <- rbind(links, as.matrix(slice[S >= threshold.upper,1:2]))
        possibleLinks <- rbind(possibleLinks,
          as.matrix(slice[S >= threshold.lower & S < threshold.upper, 1:2]))
        i <- i + n
        nPairs <- nPairs + nrow(slice)
      }
    }
    new("RLResult", data = rpairs, links = as.matrix(links),
      possibleLinks = as.matrix(possibleLinks),
      nPairs = nPairs)
  }
) # end of setMethod


setGeneric(
  name = "epiWeights",
  def = function(rpairs, e=0.01, f=getFrequencies(rpairs))
    standardGeneric("epiWeights")
)

setMethod(
  f = "epiWeights",
  signature = c("RLBigData", "ANY", "ANY"),
  definition = function (rpairs, e=0.01, f=getFrequencies(rpairs))
  {


    # Delete old weights if they exist
    # vacuum to keep file compact
    dbGetQuery(rpairs@con, "drop table if exists Wdata")
    dbGetQuery(rpairs@con, "vacuum")

    # Create a copy of the record pairs from which comparison patterns will
    # be generated. This allows concurrent writing of calculated weights.
    rpairs_copy <- clone(rpairs)
 #    dbFile2 <- tempfile(tmpdir=dirname(path.expand(rpairs@dbFile)))
#    con2 <- dbConnect(rpairs@drv, dbname = dbFile2)
#    dbGetQuery(con2, "pragma journal_mode=memory")
    # create table where weights are stored


   dbBeginTransaction(rpairs@con)

    dbGetQuery(rpairs@con, "create table Wdata (id1 integer, id2 integer, W real)")

    # create index, this speeds up the join operation of getPairs
    # significantly
    dbGetQuery(rpairs@con, "create index index_Wdata_id on Wdata (id1, id2)")
    dbGetQuery(rpairs@con, "create index index_Wdata_W on Wdata (W)")

    rpairs_copy <- begin(rpairs_copy)
    nPairs <- 0
    n <- 10000
    i = n


#    weightTable <- matrix(numeric(), ncol=3)
    expPairs <- getExpectedSize(rpairs_copy@data, rpairs@blockFld)
    pgb <- txtProgressBar(max=expPairs)


    nAttr <- ncol(rpairs_copy@data) - length(rpairs_copy@excludeFld)
    e=e+rep(0,nAttr)
    f=f+rep(0,nAttr)
    # adjust error rate
    # error rate
    w=log((1-e)/f, base=2)
    sumW <- sum(w)
      # weight computation

    row_sum <- function(r,w)
    {
      return(sum(r*w,na.rm=TRUE))
    }

    while(nrow(slice <- nextPairs(rpairs_copy, n)) > 0)
    {
#      message(i)
      flush.console()
      slice[is.na(slice)] <- 0
      #


      S=apply(slice[,-c(1,2,ncol(slice))],1,row_sum,w)/sumW
#      S=apply(slice[,-c(1,2,ncol(slice))],1,row_sum,w)/sum(w)
      if (any(is.na(S) | S < 0 | S > 1))
        warning("Some weights have illegal values. Check error rate and frequencies!")

#      weightTable <- rbind(weightTable, cbind(slice[,1:2], S))
#      dbWriteTable(con2, "Wdata", cbind(slice[,1:2], S), row.names=FALSE,
#        append=TRUE)

      dbGetPreparedQuery(rpairs@con, "insert into Wdata values (?, ?, ?)",
        cbind(slice[,1:2], S))
#      message(range(slice[,1]))
#      message(range(slice[,2]))
#      message("----------------------")
#     dbGetQuery(rpairs@con, "pragma wal_checkpoint")

      nPairs <- nPairs + nrow(slice)
      setTxtProgressBar(pgb, nPairs)
    }
    close(pgb)
    dbCommit(rpairs@con)

    # remove copied database
    clear(rpairs_copy)
    dbDisconnect(rpairs_copy@con)
    unlink(rpairs_copy@dbFile)

    rpairs
  }
) # end of setMethod
