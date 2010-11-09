#' Abstract class for large datasets
#'
#' Base class for large datasets (millions of record pairs). Each object holds
#' a database connection to store records and perform the generation of
#' comparison patterns. The database resides in a temporary RSQLite file
#' which is deleted on exit.
#'
#' @slot frequencies Relative frequency of attribute values
#' @slot blockFld Blocking definition
#' @slot excludeFld Indices of attributes to exclude from comparison
#' @slot drv Database driver. Points to a single SQLite instance
#' @slot con Database connection. Exclusive to one object
setClass(
  Class = "RLBigData",
  representation = representation(
    frequencies = "numeric",
    blockFld = "list",
    excludeFld = "integer",
    drv = "DBIDriver",
    con = "DBIConnection",
    "VIRTUAL"
  )
)    

#' Large deduplication data set
#'
#' Realization of RLBigData for deduplication of a single data set. Records are
#' stored as rows in \code{data}. Two records \code{data[i,]} and {data[j,]} are 
#' considered equal if and only if \code{identity[i]==identity[j]}
#'
#' @slot data Records to deduplicate
#' @slot identity Identity vector. 
setClass(
  Class = "RLBigDataDedup",
  contains = "RLBigData",
  representation = representation(
    data = "data.frame",
    identity = "factor"
  )
)    
#' Large linkage data set
#'
#' Realization of RLBigData for linkage of two data sets. Records are
#' stored as rows in \code{data1} and \code{data2}. Two records \code{data1[i,]} 
#' and {data2[j,]} are considered equal if and only if 
#' \code{identity1[i]==identity2[j]}
#'
#' @slot data Records to deduplicate
#' @slot identity Identity vector. 
setClass(
  Class = "RLBigDataLinkage",
  contains = "RLBigData",
  representation = representation(
    data1 = "data.frame",
    data2 = "data.frame",
    identity1 = "factor",
    identity2 = "factor"
  )
)    


RLBigDataDedup <-function(data, identity = NA, blockfld = list(), 
  exclude = integer(0))
{
 if (!is.list(blockfld) && !is.null(blockfld)) blockfld <- list(blockfld)
 if (is.character(exclude)) exclude <- match(exclude, colnames(data))
 # set up database
 drv <- dbDriver("SQLite")
 con <- dbConnect(drv, dbname="")
 coln <- make.db.names(con,colnames(data))
 # convert string indices to numeric indices
 blockfld <- lapply(blockfld, 
   function(x) {if (is.character(x)) match(x, coln) else (x)})
 object <- new("RLBigDataDedup", data=as.data.frame(data), identity=factor(identity),
  blockFld = blockfld, excludeFld = exclude, drv = drv, con = con,
  frequencies = apply(data,2,function(x) 1/length(unique(x))) )
 # write records to data base
 dbWriteTable(con, "data", cbind(data, identity))
 # calculate frequencies of attributes
 # create indices to speed up blocking
  for (blockelem in blockfld)
  {
    query <- sprintf("create index index_%s on data (%s)",
     paste(coln[blockelem], collapse="_"),
     paste(coln[blockelem], collapse=", "))
#     message(query)
  dbGetQuery(con, query)
  }
  return(object)
}
