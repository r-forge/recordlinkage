# getPairs: methods for retreiving data pairs from a result set or
# data object


# utility function

# internal function to construct SQL query for getting record pairs
# Args:
#   object: RLBigData object
#
getPairsSQL <- function(object, filter.match, filter.link, max.weight,
  min.weight, withMatch = TRUE, withClass = FALSE, withWeight = FALSE,
  sort=FALSE)
{
    stmtList <- getSQLStatement(object)
    select_list <- stmtList$select_list
    from_clause <- stmtList$from_clause
    where_clause <- stmtList$where_clause
    # get column names either from slot data or data1, depending on the class
    # (a more robust way would be good)
    colN <- switch(class(object), RLBigDataDedup = colnames(object@data),
      RLBigDataLinkage = colnames(object@data1),
      stop(paste("Unexpected class of object:", class(object))))
    # convert to database column names and add ids
    dbNames <- make.db.names(object@drv, colN, allow.keywords = FALSE)
    dbNames <- c("row_names", dbNames)
    # concatenate fields of first table, fields of second table
    # in the format t1.field1, t1.field2, ..., t2.field1, t2.field2, ...
    select_list <- paste(sapply(c("t1", "t2"), function(tableName)
        sapply(dbNames, function(fieldName) sprintf("%s.%s", tableName, fieldName))
      ), collapse=", "
    )
    if (withMatch)
    {
      select_list <- paste(select_list, "t1.identity=t2.identity as is_match",
        sep =", ")
    }
    # Include filtering by linkage result.
    # Implementation: The pairs are left joined with tables links and
    # possible_links. Links and possible links will have non-null values in the
    # columns of the corresponding table. A column "class" will be included in
    # the output which can take the values 1 (non-link), 2 (possible) and 3
    # (link), which corresponds to the factor levels "N", "P", "L"
    if (withClass)
    {
      from_clause <- paste(from_clause,
        "left join links l on (t1.row_names=l.id1 and t2.row_names=l.id2)",
      	"left join possible_links p on (t1.row_names=p.id1 and t2.row_names=p.id2)"
      )

      select_list <- paste(select_list,
        "1 + (p.id1 is not null) + (l.id1 is not null) * 2 as class",
        sep =", ")
    }

    # Include weights if desired
    # If weights have been calculated, they are stored in table 'weights'
    # in the database together with record ids. Include them by joining the
    # tables.
    if (withWeight)
    {
      if (!dbExistsTable(object@con, "weights"))
        stop(paste("No weights have been calculated for object!"))
      select_list <- paste(select_list,
        "w.weight as W", sep=","
      )
      from_clause <- paste(from_clause,
        "join weights w on (t1.row_names=w.id1 and t2.row_names=w.id2)"
      )
    }

    # add restrictions concerning matching status
    filterMatchFun <- function(filterElem)
    {
      switch(filterElem,
        match = "t1.identity=t2.identity",
        nonmatch = "t1.identity!=t2.identity",
        unknown = "t1.identity is null or t2.identity is null"
      )
    }
    # if no restriction is made (show matches, nonmatches and unknown),
    # do not add any clause
    if (any(is.na(match(c("match", "nonmatch", "unknown"), filter.match))))
    {
      filterMatch <- paste(sapply(filter.match, filterMatchFun), collapse=" or ")
    } else
    {
      filterMatch = "1"
    }


    # Add restrictions concerning classification
    # A pair is a link if the left join with the table of links gives null
    # columns (same holds for possible links)
    filterLinkFun <- function(filterElem)
    {
      switch(filterElem,
        link = "l.id1 is not null",
        possible = "p.id1 is not null",
        nonlink = "(l.id1 is null and p.id1 is null)"
      )
    }
    # if no restriction is made (show matches, nonmatches and unknown),
    # do not add any clause
    if (any(is.na(match(c("links", "nonlink", "possible"), filter.link))))
    {
      filterLink <- paste(sapply(filter.link, filterLinkFun), collapse=" or ")
    } else
    {
      filterLink = "1"
    }

    if (sort)
    {
      order_clause = "order by W"
    } else order_clause=""
    

    # return result
    sprintf("select %s from %s where %s and (%s) and (%s) %s", select_list,
      from_clause, where_clause, filterMatch, filterLink, order_clause)

}

setGeneric(
  name = "getPairs",
  def = function(object, ...) standardGeneric("getPairs")
)

setMethod(
  f = "getPairs",
  signature = "RLBigData",
  definition = function(object, filter.match = c("match", "unknown", "nonmatch"),
                        single.rows = FALSE)
  {
  
    # check arguments
    if (!is.logical(single.rows) || is.na(single.rows))
      stop(paste("Illegal value for single.rows:", single.rows))

    if (!is.character(filter.match))
      stop(paste("Illegal class for filter.match:", class(filter.match)))

    if (any(naind <- is.na(match(filter.match, c("match", "unknown", "nonmatch")))))
      stop(paste("Illegal value in filter.match:", filter.match[naind]))

    # create SQL (only from and where clause will be used)
    stmtList <- getSQLStatement(object)
    # get column names either from slot data or data1, depending on the class
    # (a more robust way would be good)
    colN <- switch(class(object), RLBigDataDedup = colnames(object@data),
      RLBigDataLinkage = colnames(object@data1),
      stop(paste("Unexpected class of object:", class(object))))
    # convert to database column names and add ids
    dbNames <- make.db.names(object@drv, colN, allow.keywords = FALSE)
    dbNames <- c("row_names", dbNames)
    # concatenate fields of first table, fields of second table
    # in the format t1.field1, t1.field2, ..., t2.field1, t2.field2, ...
    select_list <- paste(sapply(c("t1", "t2"), function(tableName)
        sapply(dbNames, function(fieldName) sprintf("%s.%s", tableName, fieldName))
      ), collapse=", "
    )
    select_list <- paste(select_list, "t1.identity=t2.identity as is_match",
      sep =", ")

    # add restrictions concerning matching status
    filterFun <- function(filterElem)
    {
      switch(filterElem,
        match = "t1.identity=t2.identity",
        nonmatch = "t1.identity!=t2.identity",
        unknown = "t1.identity is null or t2.identity is null"
      )
    }
    # if no restriction is made (show matches, nonmatches and unknown),
    # do not add any clause
    if (any(is.na(match(c("match", "nonmatch", "unknown"), filter.match))))
    {
      filterClause <- paste(sapply(filter.match, filterFun), collapse=" or ")
    } else
    {
      filterClause = "1"
    }




    stmt <- sprintf("select %s from %s where %s and (%s)", select_list,
      stmtList$from_clause, stmtList$where_clause, filterClause)
#    message(stmt)
    result <- dbGetQuery(object@con, stmt)
    colnames(result) <- c("id.1", paste(colN, ".1", sep=""), "id.2",
      paste(colN, ".2", sep=""), "is_match")
    # convert SQLite coding of boolean (0 / 1) to real logical values
    result$is_match <- as.logical(result$is_match)

    result
  }
)

setMethod(
  f = "getPairs",
  signature = "EMWeights",
  definition = function(object, max.weight = Inf, min.weight = -Inf)
  {
  }
)



setMethod(
  f = "getPairs",
  signature = "RLResult",
  definition = function(object, filter.match = c("match", "unknown", "nonmatch"),
    filter.link = c("nonlink", "possible", "link"), max.weight = Inf, min.weight = -Inf,
    withMatch = TRUE, withClass=TRUE, withWeight=FALSE, sort=withWeight)
  {
    # assing data base connection to local variable
    con <- object@data@con
    # insert match result in database and create indeces to speed up lookup
    # dbWrite does not create table if an empty data frame is passed,
    # create empty table in this case
    dbGetQuery(con, "drop table if exists links")
    dbGetQuery(con, "create table links (id1 integer, id2 integer)")
    dbGetQuery(con, "drop table if exists possible_links")
    dbGetQuery(con, "create table possible_links (id1 integer, id2 integer)")
    if (nrow(object@links) > 0)
    {
      dbWriteTable(con, "links", as.data.frame(object@links), append=TRUE,
        row.names=FALSE)
    }
    if (nrow(object@possibleLinks) > 0)
    {
      dbWriteTable(con, "possible_links", as.data.frame(object@possibleLinks), append=TRUE,
        row.names=FALSE)
    }

    dbGetQuery(object@data@con, "create index index_links on links(id1, id2)")
    dbGetQuery(object@data@con, "create index index_possible on possible_links(id1, id2)")

    stmt <- getPairsSQL(object@data, filter.match, filter.link, max.weight,
      min.weight, withMatch = withMatch, withClass = withClass,
      withWeight = withWeight, sort=sort)

    message(stmt)
    result <- dbGetQuery(con, stmt)
    # make classification result (coded as a number in 1:3) to a factor
    if (nrow(result) > 0) # otherwise class(...) throws an error
    {
      class(result$class) <- "factor"
      levels(result$class) <- c("N", "P", "L")
    }
    # make is_match logical
    result$is_match <- as.logical(result$is_match)

    # make sure column names match original names
    colN <- switch(class(object@data), RLBigDataDedup = colnames(object@data@data),
      RLBigDataLinkage = colnames(object@data@data1),
      stop(paste("Unexpected class of object:", class(object))))

    colnames(result) <- c("id.1", paste(colN, ".1", sep=""), "id.2",
      paste(colN, ".2", sep=""), "is_match", "classification")

    result
  }
)

# traditional function
setMethod(
  f = "getPairs",
  signature = "RecLinkData",
  definition = function(object, max.weight = Inf, min.weight = -Inf,
         single.rows = FALSE, show = "all", sort = !is.null(object$Wdata))
  {
    # rename object to keep old code
    rpairs <- object
    if (!("RecLinkData" %in% class(rpairs) ||
      "RecLinkResult" %in% class(rpairs)))
      stop("Wrong class for rpairs!")

    if (!is.numeric(max.weight))
      stop(paste("Illegal type for max.weight: ", class(max.weight)))

    if (!is.numeric(min.weight))
      stop(paste("Illegal type for min.weight: ", class(min.weight)))

    if (max.weight <=min.weight)
      stop("max.weight must be greater than min.weight!")

    if (!is.character(show))
      stop(paste("Illegal type for show:", class(show)))

    if (!is.element(show, c("all","links","nonlinks","possible")))
      stop(paste("Illegal value for show:", show))

    if (rpairs$type=="deduplication")
    {
        data1=rpairs$data
        data2=data1
    } else
    {
        data1=rpairs$data1
        data2=rpairs$data2
    }

  	if (!is.null(rpairs$Wdata))
    {
      ind=which(rpairs$Wdata < max.weight & rpairs$Wdata >= min.weight)
      weights <- rpairs$Wdata
    }
    else
    {
      ind <- 1:nrow(rpairs$pairs)
      weights <- rep(NA, nrow(rpairs$pairs))
    }

  	if (!is.null(rpairs$prediction))
  	{
  		show.ind=switch(show,links=which(rpairs$prediction[ind]=="L"),
  			nonlinks=which(rpairs$prediction[ind]=="N"),
   			possible=which(rpairs$prediction[ind]=="P"),
        FP=which(rpairs$prediction=="L" & rpairs$pairs$is_match==FALSE),
  			FN=which(rpairs$prediction=="N" & rpairs$pairs$is_match==TRUE),
        TRUE)
  		ind=ind[show.ind]
  	} else if (show != "all" && is.null(rpairs$prediction))
  	{
      warning("No prediction vector found, returning all data pairs!")
    }


    pairs=data.frame(Weight=weights[ind],
                    id1=rpairs$pairs[ind,1],
                    data1[rpairs$pairs[ind,1],],
                    id2=rpairs$pairs[ind,2],
                    data2[rpairs$pairs[ind,2],])


  	if (isTRUE(sort))
  	{
      	o=order(pairs$Weight,decreasing=TRUE)
      	pairs=pairs[o,]
    }

  	if (single.rows)
  	{
    	# if no pairs at all meet the restrictions, empty frame
      if (is.na(ind) || length(ind)==0)
      {
        pairs <- pairs[0,]
      }
    	colnames(pairs)=c("Weight", "id1", paste(colnames(data1),".1",sep=""),
  								   "id2", paste(colnames(data2),".2",sep=""))
  		return (pairs)
  	}

  	printfun=function(x)
    {
      c(x[1:((length(x)+1)/2)],c("",x[((length(x)+3)/2):length(x)]))

    }

    m=apply(pairs,1,printfun)
    m=as.data.frame(matrix(m[TRUE],nrow=ncol(m)*2,ncol=nrow(m)/2,byrow=TRUE))
    colnames(m)=c("Weight", "id", colnames(data1))
  	# if no pairs at all meet the restrictions, empty frame
    if (is.na(ind) || length(ind)==0)
    {
      m <- m[0,]
    }

    return(m)
  }
)


