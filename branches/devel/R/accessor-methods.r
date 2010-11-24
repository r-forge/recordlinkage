# accessor functions (get / set) for objects

setGeneric(
  name = "getFrequencies",
  def = function(x) standardGeneric("getFrequencies")
)

setMethod(
  f = "getFrequencies",
  signature = "RLBigData",
  definition = function(x) 
    if (length(x@excludeFld)!=0)
      x@frequencies[-x@excludeFld]
    else x@frequencies
)

setGeneric(
  name = "getData",
  def = function(x, ...) standardGeneric("getData")
)

setMethod(
  f = "getData",
  signature = "RLBigDataDedup",
  definition = function(x) x@data
)