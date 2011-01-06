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
  def = function(object, ...) standardGeneric("saveRLObject")
)

setMethod(
  f = "saveRLObject",
  signature = "RLBigData",
  definition = function(object, ...)
  {
  }
)

setMethod(
  f = "saveRLObject",
  signature = "RLResult",
  definition = function(object, ...)
  {
  }
)

# Load object from disk.
loadRLObject <- function(file)
{

}