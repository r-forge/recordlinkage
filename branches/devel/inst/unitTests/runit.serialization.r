.setUp <- function()
{
  data(RLdata500)
  rpairsDedup <<- RLBigDataDedup(RLdata500, identity = identity.RLdata500,
    blockfld = c(1,3))
  # use a subsample of RLdata500 as second data set in a linkage procedure
  s <- sample(500, 200)
  rpairsLinkage <<- RLBigDataLinkage(RLdata500, RLdata500[s,],
    identity1 = identity.RLdata500, identity2 = identity.RLdata500[s],
    blockfld = 1)
}

# utility function to compare an object with its clone
compareRLBigData <- function(org, copy)
{
  # check if all slots of the copy have the same value except the
  # database connection and file
  checkEquals(class(org), class(copy), msg = " check that copy has same class")
  slotN <- slotNames(class(org))
  for (s in slotN)
  {
    if (s %in% c("con", "dbFile"))
    {
      checkTrue(!identical(slot(org, s), slot(copy, s)),
        msg = sprintf(" check that slot %s differs in copy", s))
    } else if (s=="drv")
    {
      # no all.equal method for external pointers - use identical instead
      checkTrue(identical(slot(org, s), slot(copy, s)),
        msg = sprintf(" check that slot %s is equal in copy", s))
    } else
    {
      checkEquals(slot(org, s), slot(copy, s),
        msg = sprintf(" check that slot %s is equal in copy", s))

    }
  }

  # check that databases have the same content after copying
  for (tab in dbListTables(org@con))
  {
    tabOrg <- dbReadTable(org@con, tab)
    tabClone <- dbReadTable(copy@con, tab)
    checkEquals(tabOrg, tabClone,
      msg = sprintf(" check that table %s has same content in copy", tab))
  }

}

test.clone.RLBigDataDedup <- function()
{
  rpairsDedupClone <- clone(rpairsDedup)

  # check equality (by value) via utility function
  compareRLBigData(rpairsDedup, rpairsDedupClone)

  # check that alteration of the copy does not affect the original
  tabBefore <- dbReadTable(rpairsDedup@con, "data")
  dbGetQuery(rpairsDedupClone@con, "update data set 'bm'='bm' + 1")
  tabAfter <- dbReadTable(rpairsDedup@con, "data")
  checkEquals(tabBefore, tabAfter, msg = paste(" check that original database",
    "is not affected by change in copy"))
}


test.clone.RLBigDataLinkage <- function()
{
  rpairsLinkageClone <- clone(rpairsLinkage)

  # check equality (by value) via utility function
  compareRLBigData(rpairsLinkage, rpairsLinkageClone)


  # check that alteration of the copy does not affect the original
  tabBefore <- dbReadTable(rpairsLinkage@con, "data1")
  dbGetQuery(rpairsLinkageClone@con, "update data1 set 'bm'='bm' + 1")
  tabAfter <- dbReadTable(rpairsLinkage@con, "data1")
  checkEquals(tabBefore, tabAfter, msg = paste(" check that original database",
    "is not affected by change in copy"))
}

# Test for save and load functionality in one function
test.saveLoad.RLBigDataDedup <- function()
{
  # save object
  file <- tempfile()
  saveRLObject(rpairsDedup, file = file)

  # reload into different variable
  rpairsDedupReload <- loadRLObject(tempfile())
  
  # compare objects
  compareRLBigData(rpairsDedup, rpairsDedupReload)
}