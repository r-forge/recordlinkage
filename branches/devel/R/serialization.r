# functions and methods to clone Record Linkage objects and save them
# to disk (applying to RL* object which store data in a database)

# Clone object, including database.
setGeneric(
  name = "clone",
  def = function(object, ...) standardGeneric("clone")
)

setMethod(
  f = "clone",
  signature = "RLBigData",
  definition = function(object, ...)
  {
    result <- object
    result@dbFile <- tempfile()
    sqliteCopyDatabase(from = object@con, to = result@dbFile)
    result@con <- dbConnect(result@drv, dbname = result@dbFile)
    result
  }
)

setMethod(
  f = "clone",
  signature = "RLResult",
  definition = function(object, ...)
  {
  }
)


# Save object to disk. File format is a SQLite database with
# additional table to hold the R object
setGeneric(
  name = "saveRLObject",
  def = function(object, file, ...) standardGeneric("saveRLObject")
)

setMethod(
  f = "saveRLObject",
  signature = "RLBigData",
  definition = function(object, file, ...)
  {
    # first, save existing database and open for the following operations
    sqliteCopyDatabase(from = object@con, to = file)
    target <- dbConnect(object@drv, file)
    
    # serialize R object and store in a BLOB
    serializedObj <- serialize(object, connection = NULL)
    dbGetQuery(target, "create table serialization (data blob)")
    dbGetPreparedQuery(target, "insert into serialization values (:val)",
      data.frame(val=I(list(serializedObj))))
    dbDisconnect(target)
  }
)

setMethod(
  f = "saveRLObject",
  signature = "RLResult",
  definition = function(object, file, ...)
  {
  }
)

# Load object from disk.
loadRLObject <- function(file)
{
  # open connection and verify that a table serializatin exists
  # (this is where the object is saved)
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname = file)
  if (!dbExistsTable(con, "serialization"))
    stop(sprintf("Could not find serialized R object in database file %s!", file))

  # Load object. The returned data frame holds the serialized object as a
  # raw vector packed in a list
  object <- unserialize(dbReadTable(con, "serialization")[1,1][[1]])
  if (!inherits(object, c("RLBigData", "RLResult")))
    stop(sprintf("Found an object of unsupported class: %s", class(object)))

  # copy database to temporary file and delete serialization in db
  newfile <- tempfile()
  sqliteCopyDatabase(from = con, to = newfile)
  dbDisconnect(con)
  con <- dbConnect(drv, dbname=newfile)
  dbGetQuery(con, "drop table serialization")

  # attach connection info to object
  if(inherits(object, "RLBigData"))
  {
    object@dbFile <- newfile
    object@drv <- drv
    object@con <- con
  } else if (inherits(object, "RLResult"))
  {
    object@data@dbFile <- newfile
    object@data@drv <- drv
    object@data@con <- con
  }
  object
}