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
    excludeFld = "numeric",
    strcmpFld = "numeric",
    strcmpFun = "character",
    phoneticFld = "numeric",
    phoneticFun ="character",
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

# constructor
RLBigDataDedup <- function(data, identity = NA, blockfld = list(), 
  exclude = numeric(0), strcmp = numeric(0), 
  strcmpfun = "jarowinkler", phonetic=numeric(0), phonfun = "pho_h")
{
  # if strings are used to identify columns, convert to numeric indices
  if (is.character(exclude)) exclude <- match(exclude, colnames(data))
  if (is.character(strcmp)) strcmp <- match(strcmp, colnames(data))
  if (is.character(phonetic)) phonetic <- match(phonetic, colnames(data))
  
  # if strcmp or phonetic is TRUE, set it to all existing columns
  # excluded fields are omitted during construction of SQL commands
  if (isTRUE(strcmp)) strcmp = 1:ncol(data)
  if (isTRUE(phonetic)) phonetic = 1:ncol(data)
  
  # put blockfld into list if necessary, convert string indices to numeric indices
  if (!is.list(blockfld) && !is.null(blockfld)) blockfld <- list(blockfld)
  blockfld <- lapply(blockfld, 
   function(x) {if (is.character(x)) match(x, colnames(data)) else (x)})

  # construct column names if not assigned
  if (is.null(colnames(data)))
  colnames(data)=paste("V", 1:ncol(data), sep="")

  # set up database
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname="")
  coln <- make.db.names(con,colnames(data))

  # construct object  
  object <- new("RLBigDataDedup", data=as.data.frame(data), identity=factor(identity),
    blockFld = blockfld, excludeFld = exclude, strcmpFld = strcmp,
    strcmpFun = strcmpfun, phoneticFld = phonetic, phoneticFun = phonfun,
    drv = drv, con = con, frequencies = apply(data,2,function(x) 1/length(unique(x))) )

  # write records to database
  dbWriteTable(con, "data", data.frame(data, identity = identity))

  # create indices to speed up blocking
  for (blockelem in blockfld)
  {
    query <- sprintf("create index index_%s on data (%s)",
     paste(coln[blockelem], collapse="_"),
     paste(coln[blockelem], collapse=", "))
    dbGetQuery(con, query)
  }
  # create index on identity vector to speed up identifying true matches
  dbGetQuery(con, "create index index_identity on data (identity)")
  # init extension functions (string comparison, phonetic code) for SQLite
  init_sqlite_extensions(con)
  return(object)
}


# constructor
RLBigDataLinkage <-function(data1, identity1 = NA, data2, identity2 = NA, blockfld = list(), 
  exclude = numeric(0), strcmp = numeric(0), 
  strcmpfun = "jarowinkler", phonetic=numeric(0), phonfun = "pho_h")
{
 # if strings are used to identify columns, convert to numeric indices
 if (is.character(exclude)) exclude <- match(exclude, colnames(data))
 if (is.character(strcmp)) strcmp <- match(strcmp, colnames(data))
 if (is.character(phonetic)) phonetic <- match(phonetic, colnames(data))

 # if strcmp or phonetic is TRUE, set it to all existing columns
 # excluded fields are omitted during construction of SQL commands
 if (isTRUE(strcmp)) strcmp = 1:ncol(data)
 if (isTRUE(phonetic)) phonetic = 1:ncol(data)
 
 # construct column names if not assigned
 if (is.null(colnames(data)))
  colnames(data)=paste("V", 1:ncol(data), sep="")
 # set up database
 drv <- dbDriver("SQLite")
 con <- dbConnect(drv, dbname="")
 coln <- make.db.names(con,colnames(data))

 # convert string indices to numeric indices
 if (!is.list(blockfld) && !is.null(blockfld)) blockfld <- list(blockfld)
 blockfld <- lapply(blockfld, 
   function(x) {if (is.character(x)) match(x, coln) else (x)})
 object <- new("RLBigDataDedup", data=as.data.frame(data), identity=factor(identity),
  blockFld = blockfld, excludeFld = exclude, strcmpFld = strcmp,
  strcmpFun = strcmpfun, phoneticFld = phonetic, phoneticFun = phonfun,
  drv = drv, con = con, frequencies = apply(data,2,function(x) 1/length(unique(x))) )
 # write records to data base
 dbWriteTable(con, "data", data.frame(data, identity = identity))
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
  # create index on identity vector to speed up identifying true matches
  dbGetQuery(con, "create index index_identity on data (identity)")
  # init extension functions (string comparison, phonetic code) for SQLite
  init_sqlite_extensions(con)
  return(object)
}
