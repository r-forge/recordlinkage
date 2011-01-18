# internals.r: Various utility functions which are (usually) not called from
# outside the package


### Functions to create SQL statements and to retreive record pairs from
### the database

# construct part of where-clause which represents blocking restrictions
# make something like 'list(1:2, 3)' to something like
# '(t1.field1=t2.field1 and t1.field2=t2.field2) or (t1.field3=t2.field3)'
blockfldfun <- function(blockfld, phoneticFld, phoneticFun, coln)
{
  blockElemFun <- function(fldIndex)
  {
    if (fldIndex %in% phoneticFld)
      return(sprintf("%1$s(t1.'%2$s')=%1$s(t2.'%2$s')", phoneticFun, coln[fldIndex]))
    else
      return(sprintf("t1.'%1$s'=t2.'%1$s'", coln[fldIndex]))
  }

 paste("(", paste(sapply(blockfld, function(blockvec)
                  paste(sapply(blockvec, blockElemFun),
                        collapse=" and ")),
                  collapse=") or ("), ")", sep="")
}

#' Create SQL statement
#'
#' Creates SQL statememt to retreive comparison patterns, respecting
#' parameters such as blocking definition and exclusion of fields.
#'
#' @value A list with components "select_list", "from_clause", "where_clause"
#' representing the corresponding parts of the query without the keywords
#' 'SELECT', 'FROM' and 'WHERE'.
setGeneric(
  name = "getSQLStatement",
  def = function(object) standardGeneric("getSQLStatement")
)

setMethod(
  f = "getSQLStatement",
  signature = "RLBigData",
  definition = function(object)
  {
    # constructs select for a single column, to be used by lapply 
    # (see below)
    selectListElem <- function(fldIndex, coln, excludeFld, strcmpFld, strcmpFun,
                              phoneticFld, phoneticFun)
    {
      # nothing if field is excluded
      if (fldIndex %in% excludeFld)
        return(character(0))
        
      # enclose fields in phonetic function if desired
      if (fldIndex %in% phoneticFld)
      {
        fld1 <- sprintf("%s(t1.%s)", phoneticFun, coln[fldIndex])
        fld2 <- sprintf("%s(t2.%s)", phoneticFun, coln[fldIndex])
      } else
      {
        fld1 <- sprintf("t1.%s", coln[fldIndex])
        fld2 <- sprintf("t2.%s", coln[fldIndex])
      }


      # something like 'jarowinkler(t1.fname, t2.fname) as fname'
      if (fldIndex %in% strcmpFld)
        return(sprintf("%s(%s, %s) as %s", strcmpFun, fld1, fld2, coln[fldIndex]))


      # direct comparison: something like 't1.fname=t2.fname as fname'      
      return(sprintf("%s=%s as %s", fld1, fld2, coln[fldIndex]))
    }
    coln <- switch(class(object),
      RLBigDataDedup = make.db.names(object@con, colnames(object@data),
        keywords = SQLKeywords(object@drv)),
      RLBigDataLinkage = make.db.names(object@con, colnames(object@data1),
        keywords = SQLKeywords(object@drv)))
    selectlist_id <- "t1.row_names as id1, t2.row_names as id2"
    # use unlist to delete NULLs from list
    selectlist <- paste(unlist(lapply(1:length(coln), selectListElem,
      coln, object@excludeFld, object@strcmpFld, object@strcmpFun,
      object@phoneticFld, object@phoneticFun)), collapse = ", ")
    selectlist <- paste(selectlist, "t1.identity=t2.identity as is_match", sep=",")
    fromclause <- switch(class(object), RLBigDataDedup = "data t1, data t2",
                                        RLBigDataLinkage = "data1 t1, data2 t2")
    whereclause <- switch(class(object), RLBigDataDedup = "t1.row_names < t2.row_names",
                                        RLBigDataLinkage = "1")
#    if (length(object@excludeFld) > 0)
#      coln <- coln[-object@excludeFld]
    if (length(object@blockFld)>0)
    {
     whereclause <- sprintf("%s and (%s)", whereclause, blockfldfun(object@blockFld,
      object@phoneticFld, object@phoneticFun, coln))
    }
    return(list(select_list = paste(selectlist_id, selectlist, sep=", "),
                from_clause = fromclause, where_clause = whereclause) )
  }
)


#' Begin generation of data pairs
#'
#' An SQL statement representing the generation of data pairs, including
#' the configuration of blocking fields, phonetics etc. is constructed and
#' send to SQLite.
setGeneric(
  name = "begin",
  def = function(x, ...) standardGeneric("begin")
)
      
setMethod(
  f = "begin",
  signature = "RLBigData",
  definition = function(x, ...)
  {
    sql <- getSQLStatement(x)  
    query <- sprintf("select %s from %s where %s", sql$select_list, 
      sql$from_clause, sql$where_clause)
    dbSendQuery(x@con, query) # can be retreived via dbListResults(x@con)[[1]]
    return(x)
  }
)

# retreive next n pairs
setGeneric(
  name = "nextPairs",
  def = function(x, n=10000, ...) standardGeneric("nextPairs")
)

setMethod(
  f = "nextPairs",
  signature = "RLBigData",
  definition = function(x, n=10000, ...)
  {
    res <- dbListResults(x@con)[[1]]
    result <- fetch(res, n)
    # Spalten, die nur NA enthalten, werden als character ausgegeben, deshalb
    # Umwandlung nicht-numerischer Spalten in numeric
    if (nrow(result) > 0) # wichtig, weil sonst Fehler bei Zugriff auf Spalten auftritt
    {
      for (i in 1:ncol(result))
      {
        if (!is.numeric(result[,i]))
          result[,i] <- as.numeric(result[,i])
      }
    }
    result
  }
)

# Clean up after retreiving data pairs (close result set in database)
setGeneric(
  name = "clear",
  def = function(x, ...) standardGeneric("clear")
)

setMethod(
  f = "clear",
  signature = "RLBigData",
  definition = function(x, ...) dbClearResult(dbListResults(x@con)[[1]])
)


### Functions neccessary to load extensions into SQLite database

# Function body taken from init_extension, package RSQLite.extfuns
init_sqlite_extensions <- function(db)
{
    ans <- FALSE
    if (.allows_extensions(db)) {
        res <- dbGetQuery(db, sprintf("SELECT load_extension('%s')",
                                      .lib_path()))
        ans <- all(dim(res) == c(1, 1))
    } else {
        stop("loadable extensions are not enabled for this db connection")
    }
    ans
}

# taken from RSQLite.extfuns
.allows_extensions <- function(db)
{
    v <- dbGetInfo(db)[["loadableExtensions"]]
    isTRUE(v) || (v == "on")
}


# taken from RSQLite.extfuns
.lib_path <- function()
{
    ## this is a bit of a trick, but the NAMESPACE code
    ## puts .packageName in the package environment and this
    ## seems slightly better than hard-coding.
    ##
    ## This also relies on the DLL being loaded even though only SQLite
    ## actually needs to load the library.  It does not appear that
    ## loading it causes any harm and it makes finding the path easy
    ## (don't have to worry about arch issues).
    getLoadedDLLs()[[.packageName]][["path"]]
}


### Internal getter-functions

# get count of each distinct comparison pattern
# Fuzzy values above cutoff are converted to 1, below cutoff to 0
# NAs are converted to 0
setGeneric(
  name = "getPatternCounts",
  def = function(x, n=10000, cutoff=1) standardGeneric("getPatternCounts")
)

setMethod(
  f = "getPatternCounts",
  signature = "RLBigData",
  definition = function(x, n=10000, cutoff=1)
  {
   on.exit(clear(x))
   x <- begin(x)
   patternCounts <- 0L
   i = n
   while(nrow(slice <- nextPairs(x, n)) > 0)
   {
    message(i)
    flush.console()
    # discard ids and matching status
    slice <- slice[,-c(1,2,ncol(slice))]
    slice[is.na(slice)] <- 0
    slice[slice < cutoff] <- 0
    slice[slice >= cutoff] <- 1
    patternCounts <- patternCounts + countpattern(slice)
     i <- i + n
   }      
   patternCounts
  }
)

# get number of matches
setGeneric(
  name = "getMatchCount",
  def = function(object) standardGeneric("getMatchCount")
)

setMethod(
  f = "getMatchCount",
  signature = "RLBigData",
  definition = function(object)
  {
    sql <- getSQLStatement(object)
    sql_stmt <- sprintf(
      "select count(*) from %s where %s and t1.identity==t2.identity",
      sql$from_clause, sql$where_clause)
    return(as.integer(dbGetQuery(object@con, sql_stmt)))
  }
) 


# Get the number of pairs with unknown matching status
setGeneric(
  name = "getNACount",
  def = function(object) standardGeneric("getNACount")
)

setMethod(
  f = "getNACount",
  signature = "RLBigData",
  definition = function(object)
  {
    sql <- getSQLStatement(object)
    sql_stmt <- sprintf(
      "select count(*) from %s where %s and (t1.identity is null or t2.identity is null)",
      sql$from_clause, sql$where_clause)
    return(as.integer(dbGetQuery(object@con, sql_stmt)))
  }
)


### Various other utility functions

# utility function to generate all unordered pairs of x[1]..x[n]
# if x is a vector, or 1..x
unorderedPairs <- function (x)
{
    if (length(x)==1)
    {
      if (!is.numeric(x) || x < 2)
        stop("x must be a vector or a number >= 2")
        return (array(unlist(lapply(1:(x-1),
          function (k) rbind(k,(k+1):x))),dim=c(2,x*(x-1)/2)))
    }
    if (!is.vector(x))
      stop ("x must be a vector or a number >= 2")
    n=length(x)
    return (array(unlist(lapply(1:(n-1),
    function (k) rbind(x[k],x[(k+1):n]))),dim=c(2,n*(n-1)/2)))
}

isFALSE <- function(x) identical(x,FALSE)

delete.NULLs  <-  function(x)
    x[unlist(lapply(x, length) != 0)]

# interprets a scalar x as a set with 1 element (see also man page for sample)
resample <- function(x, size, ...)
     if(length(x) <= 1) { if(!missing(size) && size == 0) x[FALSE] else x
     } else sample(x, size, ...)

# estimate the number of record pairs
setGeneric(
  name = "getExpectedSize",
  def = function(object, ...) standardGeneric("getExpectedSize")
)

setMethod(
  f = "getExpectedSize",
  signature = "data.frame",
  definition = function(object, blockfld=list())
  {
    if(!is.list(blockfld)) blockfld = list(blockfld)
    rpairs <- RLBigDataDedup(object)
    nData <- nrow(object)
    nAll <- nData * (nData - 1) / 2
    if (length(blockfld)==0) return(nAll)
    coln <- make.db.names(rpairs@con, colnames(object))

    # ergibt Wahrscheinlichkeit, dass mit gegebenen Blockingfeldern
    # ein Paar nicht gezogen wird
    blockelemFun <- function(blockelem)
    {
      if(is.character(blockelem)) blockelem <- match(blockelem, colnames(object))
      freq <- dbGetQuery(rpairs@con,
        sprintf("select count(*) as c from data group by %s having c > 1 and %s",
          paste("\"", coln[blockelem], "\"", sep="", collapse=", "),
          paste(
            sapply(coln[blockelem], sprintf, fmt = "\"%s\" is not null"),
            collapse = " and "
          )
        )
      )
      1 - (sum(sapply(freq,  function(x) x * (x-1) /2)) / nAll)
    }
    res <- nAll * (1-prod(sapply(blockfld, blockelemFun)))

    # avoid clutter from temporary files
    dbDisconnect(rpairs@con)
    unlink(rpairs@dbFile)

    res
  }
)

setMethod(
  f = "getExpectedSize",
  signature = "RLBigDataDedup",
  definition = function(object)
  {
    blockfld <- object@blockFld
    if(!is.list(blockfld)) blockfld <- list(blockfld)
    nData <- nrow(object@data)
    nAll <- nData * (nData - 1) / 2
    if (length(blockfld)==0) return(nAll)
    coln <- make.db.names(object@con, colnames(object@data))

    # ergibt Wahrscheinlichkeit, dass mit gegebenen Blockingfeldern
    # ein Paar nicht gezogen wird
    blockelemFun <- function(blockelem)
    {
      if(is.character(blockelem)) blockelem <- match(blockelem, colnames(object@data))
      freq <- dbGetQuery(object@con,
        sprintf("select count(*) as c from data group by %s having c > 1 and %s",
          paste("\"", coln[blockelem], "\"", sep="", collapse=", "),
          paste(
            sapply(coln[blockelem], sprintf, fmt = "\"%s\" is not null"),
            collapse = " and "
          )
        )
      )
      1 - (sum(sapply(freq,  function(x) x * (x-1) /2)) / nAll)
    }
    res <- nAll * (1-prod(sapply(blockfld, blockelemFun)))


    res
  }
)

setMethod(
  f = "getExpectedSize",
  signature = "RLBigDataLinkage",
  definition = function(object)
  {
    blockfld <- object@blockFld
    if(!is.list(blockfld)) blockfld <- list(blockfld)
    nData1 <- nrow(object@data1)
    nData2 <- nrow(object@data2)
    nAll <- nData1 * nData2
    if (length(blockfld)==0) return(nAll)
    coln <- make.db.names(object@con, colnames(object@data1))

    # ergibt Wahrscheinlichkeit, dass mit gegebenen Blockingfeldern
    # ein Paar nicht gezogen wird
    blockelemFun <- function(blockelem)
    {
      if(is.character(blockelem)) blockelem <- match(blockelem, colnames(object@data))
      freq <- dbGetQuery(object@con,
        sprintf("select count(*) as c from data1 t1, data2 t2 where %s",
          paste(
            sapply(coln[blockelem], sprintf, fmt = "t1.\"%1$s\"=t2.\"%1$s\""),
            collapse = " and "
          )
        )
      )$c
      1 - (freq / nAll)
    }
    res <- nAll * (1-prod(sapply(blockfld, blockelemFun)))
    res
  }
)

