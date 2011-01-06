.setUp <- function()
{
  data(RLdata500)
  assign("rpairsDedup", RLBigDataDedup(RLdata500, identity = identity.RLdata500,
    blockfld = c(1,3)), parent.frame())
  # use a subsample of RLdata500 as second data set in a linkage procedure
  s <- sample(500, 200)
  assign("rpairsLinkage", RLBigDataLinkage(RLdata500, RLdata500[s,],
    identity1 = identity.RLdata500, identity2 = identity.RLdata500[s],
    blockfld = 1), parent.frame())
}

test.clone-RLBigDataDedup <- function()
{
  rpairsDedupClone <- clone(rpairsDedup)
  # check if all slots of the copy have the same value except the
  # database connection and file
  slotN <- slotNames("RLBigDataDedup")
  for (s in slotN)
  {
    if (s %in% c("con", "dbFile")
    {
      checkTrue(!identical(getSlot(rpairsDedup, s), getSlot(rpairsDedupClone, s)),
        msg = sprintf(" check that slot %s differs in copy", s))
    } else
    {
      checkEquals(getSlot(rpairsDedup, s), getSlot(rpairsDedupClone, s),
        msg = sprintf(" check that slot %s is equal in copy", s))

    }
  }
  
  # check that databases have the same content after copying
  for (tab in dbListTables(rpairsDedup@con))
  {
    tabOrg <- dbReadTable(rpairsDedup@con, tab)
    tabClone <- dbReadTable(rpairsDedupClone@con, tab)
    checkEquals(tabOrg, tabClone,
      msg = sprintf(" check that table %s has same content in copy", tab))
  }
  
  # check that alteration of the copy does not affect the original
  tabBefore <- dbReadTable(rpairsDedup@con, "data")
  dbGetQuery(rpairsDedupClone@con, "update data set 'bm'='bm' + 1")
  tabAfter <- dbReadTable(rpairsDedup@con, "data")
  checkEquals(tabBefor, tabAfter, msg = paste(" check that original database",
    "is not affected by change in copy"))
}