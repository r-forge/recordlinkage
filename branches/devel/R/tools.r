# tools.r: varios utility functions

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

resample <- function(x, size, ...)
     if(length(x) <= 1) { if(!missing(size) && size == 0) x[FALSE] else x
     } else sample(x, size, ...)

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