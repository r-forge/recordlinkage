.setUp <- function()
{
  # data used for the test
  data1 <<- read.table("data1.getPairs.txt", sep=",", na.strings="",header=TRUE)
  identity1 <<- scan("identity1.getPairs.txt",comment.char="#",sep=",")
}

# Test behaviour for illegal input
test.getPairs.RLBigDataDedup.exceptions <- function()
{
  rpairs <- RLBigDataDedup(data1, identity = identity1)

  # object: no test, integrity enforced by method dispatching and
  # (still to be written) inspector
  
  # filter.match
  # illegal class
  checkException(getPairs(rpairs, filter.match = c(1, 3)))
  checkException(getPairs(rpairs, filter.match = list("match")))
  # illegal values
  checkException(getPairs(rpairs, filter.match = c("link")))
  checkException(getPairs(rpairs, filter.match = c("match", "link")))

  # single.rows
  
  
}

test.getPairs.RLBigDataDedup <- function()
{
  # set up test object
  rpairs <- RLBigDataDedup(data1, identity = identity1)
  reqResult <- read.table("result1.getPairs.txt",sep=",",header=TRUE, colClasses="factor")


  # The resulting set consists of 6 data pairs
  # call with default values
# TODO

  # test with sinlge.rows=FALSE
  # column classes are chosen so that text fields are factors and number fields
  # (including date of birth) are integer
  
  # but: column classes are ignored as it is difficult to enforce
  # the same format as in the data (conversion by SQLite engine)
  reqResult <- read.table("result2.getPairs.txt",sep=",",header=TRUE,
    colClasses = c(rep(c("integer", rep("factor", 4), rep("integer", 3)),2),
    "logical"))
  testResult <- getPairs(rpairs, single.rows = FALSE)
  # assign explicit row names
  rownames(reqResult) <- rownames(reqResult)
  checkEquals(as.matrix(testResult), as.matrix(reqResult), msg = " all pairs on single rows")
  
  # tests for argument filter.match (and single.rows=FALSE for easier coding)
  #
  # The table of expected results (reqTable) holds pairs with the following
  # status: T F NA F NA NA. In the following, the slice with "allowed"
  # outcome is selected and compared to the output of getPairs()
  result2 <- reqResult
  
  # 1. show only non-matches
  testResult <- getPairs(rpairs, single.rows = FALSE, filter.match="nonmatch")
  reqResult <- result2[c(2,4),]
  rownames(reqResult) <- 1:nrow(reqResult)
  checkTrue(all(testResult$is_match==FALSE),
    msg=" only non-matches")
  # check also the prepared test data for consistency
  checkTrue(all(reqResult$is_match==FALSE),
    msg=" only non-matches")
  checkEquals(as.matrix(testResult), as.matrix(reqResult),
    msg=" only non-matches")

  # 2. show only matches
  testResult <- getPairs(rpairs, single.rows = FALSE, filter.match="match")
  reqResult <- result2[1,]
  rownames(reqResult) <- 1:nrow(reqResult)
  checkTrue(all(testResult$is_match==TRUE),
    msg=" only matches")
  # check also the prepared test data for consistency
  checkTrue(all(reqResult$is_match==TRUE),
    msg=" only matches")
  checkEquals(as.matrix(testResult), as.matrix(reqResult),
    msg=" only matches")

  # 3. show only unknown pairs
  testResult <- getPairs(rpairs, single.rows = FALSE, filter.match="unknown")
  reqResult <- result2[c(3,5,6),]
  rownames(reqResult) <- 1:nrow(reqResult)
  checkTrue(all(is.na(testResult$is_match)),
    msg=" only unknown")
  # check also the prepared test data for consistency
  checkTrue(all(is.na(reqResult$is_match)),
    msg=" only unknown")
  checkEquals(as.matrix(testResult), as.matrix(reqResult),
    msg=" only unknown")

  # 4. show non-matches and unknown
  testResult <- getPairs(rpairs, single.rows = FALSE,
    filter.match=c("nonmatch","unknown"))
  reqResult <- result2[2:6,]
  rownames(reqResult) <- 1:nrow(reqResult)
  checkTrue(all(is.na(testResult$is_match) | testResult$is_match==FALSE),
    msg=" non-match or unknown")
  # check also the prepared test data for consistency
  checkTrue(all(is.na(reqResult$is_match) | reqResult$is_match==FALSE),
    msg=" non-match or unknown")
  checkEquals(as.matrix(testResult), as.matrix(reqResult),
    msg=" non-match or unknown")

  # 5. show matches and unknown
  testResult <- getPairs(rpairs, single.rows = FALSE,
    filter.match=c("match","unknown"))
  reqResult <- result2[c(1,3,5,6),]
  rownames(reqResult) <- 1:nrow(reqResult)
  checkTrue(all(is.na(testResult$is_match) | testResult$is_match==TRUE),
    msg=" match or unknown")
  # check also the prepared test data for consistency
  checkTrue(all(is.na(reqResult$is_match) | reqResult$is_match==TRUE),
    msg=" match or unknown")
  checkEquals(as.matrix(testResult), as.matrix(reqResult),
    msg=" match or unknown")

  # 6. show matches and non-matches
  testResult <- getPairs(rpairs, single.rows = FALSE,
    filter.match=c("match","nonmatch"))
  reqResult <- result2[c(1,2,4),]
  rownames(reqResult) <- 1:nrow(reqResult)
  checkTrue(all(testResult$is_match==FALSE | testResult$is_match==TRUE),
    msg=" match or non-match")
  # check also the prepared test data for consistency
  checkTrue(all(reqResult$is_match==FALSE | reqResult$is_match==TRUE),
    msg=" match or non-match")
  checkEquals(as.matrix(testResult), as.matrix(reqResult),
    msg=" match or non-match")

  # check that empty result does not throw an error
  identity2 <- 1:4 # all records are different -> no matches
  rpairs <- RLBigDataDedup(data1, identity = identity2)
  testResult <- getPairs(rpairs, single.rows = FALSE, filter.match = "match")
  checkEquals(as.matrix(testResult), as.matrix(result2[NULL,]),
    msg = " empty result 1")
  testResult <- getPairs(rpairs, single.rows = FALSE,
    filter.match = c("match", "unknown"))
  checkEquals(as.matrix(testResult), as.matrix(result2[NULL,]),
    msg = " empty result 2")

  identity2 <- rep(NA, 4) # only "unknown" pairs
  rpairs <- RLBigDataDedup(data1, identity = identity2)
  testResult <- getPairs(rpairs, single.rows = FALSE, filter.match = "nonmatch")
  checkEquals(as.matrix(testResult), as.matrix(result2[NULL,]),
    msg = " empty result 3")
  testResult <- getPairs(rpairs, single.rows = FALSE,
    filter.match = c("match", "nonmatch"))
  checkEquals(as.matrix(testResult), as.matrix(result2[NULL,]),
    msg = " empty result 4")

  
  # check column names if database names are different
  colnames(data1)=c("fname.c1", "fname.c2", "lname.c1", "lname.c2", "by", "where", "select")
  rpairs <- RLBigDataDedup(data1, identity = identity2)
  testResult <- getPairs(rpairs)
  checkEquals(colnames(testResult), c("id.1", paste(colnames(data1), ".1", sep=""),
    "id.2", paste(colnames(data1), ".2", sep=""), "is_match"),
    msg = " check column names")
}


test.getPairs.RLResult.exceptions <- function()
{
  # single.rows


}



# better: test of getPairs in order to be independent of actual SQL
# realization
#test.getPairsSQL <- function()
#{
#  # create dataset with reduced set of attributes (to shorten SQL strings)
#  smallData <- data1[,c(1,5)]
#  colnames(smallData) <- c("a", "b")
#  rpairs <- RLBigDataDedup(smallData, identity1)
#
#  # unrestricted
#  testResult <- getPairsSQL(
#    rpairs,
#    filter.match = c("match", "non-match", "unknown"),
#    filter.link = c("link", "nonlink", "possible"),
#    max.weight = Inf,
#    min.weight = -Inf
#  )
#  checkEquals(testResult, paste"select t1.id as id.1, t1.a as a.1, t2.b as b.1,"
#
#
#}

test.getPairs.RLResult <- function()
{


  # set up test object
  # match status of test pairs is: T F NA F NA NA
  # link status set to:            L L L  N N  N
  rpairs <- RLBigDataDedup(data1, identity = identity1)
  result <- new("RLResult", data = rpairs,
    links = matrix(c(1,2,1,3,1,4), ncol = 2, byrow = TRUE),
    possibleLinks=matrix(ncol=2, nrow=0), nPairs = 6)


  # get


  # this gives the following combinations:





  reqResult <- read.table("result1.getPairs.txt",sep=",",header=TRUE, colClasses="factor")


  # The resulting set consists of 6 data pairs
  # call with default values
# TODO

  # test with sinlge.rows=FALSE
  # column classes are chosen so that text fields are factors and number fields
  # (including date of birth) are integer

  # but: column classes are ignored as it is difficult to enforce
  # the same format as in the data (conversion by SQLite engine)
  reqResult <- read.table("result2.getPairs.txt",sep=",",header=TRUE,
    colClasses = c(rep(c("integer", rep("factor", 4), rep("integer", 3)),2),
    "logical"))
  testResult <- getPairs(rpairs, single.rows = FALSE)
  # assign explicit row names
  rownames(reqResult) <- rownames(reqResult)
  checkEquals(as.matrix(testResult), as.matrix(reqResult), msg = " all pairs on single rows")

  # tests for argument filter.match (and single.rows=FALSE for easier coding)
  #
  # The table of expected results (reqTable) holds pairs with the following
  # status: T F NA F NA NA. In the following, the slice with "allowed"
  # outcome is selected and compared to the output of getPairs()
  result2 <- reqResult

  # 1. show only non-matches
  testResult <- getPairs(rpairs, single.rows = FALSE, filter.match="nonmatch")
  reqResult <- result2[c(2,4),]
  rownames(reqResult) <- 1:nrow(reqResult)
  checkTrue(all(testResult$is_match==FALSE),
    msg=" only non-matches")
  # check also the prepared test data for consistency
  checkTrue(all(reqResult$is_match==FALSE),
    msg=" only non-matches")
  checkEquals(as.matrix(testResult), as.matrix(reqResult),
    msg=" only non-matches")

  # 2. show only matches
  testResult <- getPairs(rpairs, single.rows = FALSE, filter.match="match")
  reqResult <- result2[1,]
  rownames(reqResult) <- 1:nrow(reqResult)
  checkTrue(all(testResult$is_match==TRUE),
    msg=" only matches")
  # check also the prepared test data for consistency
  checkTrue(all(reqResult$is_match==TRUE),
    msg=" only matches")
  checkEquals(as.matrix(testResult), as.matrix(reqResult),
    msg=" only matches")

  # 3. show only unknown pairs
  testResult <- getPairs(rpairs, single.rows = FALSE, filter.match="unknown")
  reqResult <- result2[c(3,5,6),]
  rownames(reqResult) <- 1:nrow(reqResult)
  checkTrue(all(is.na(testResult$is_match)),
    msg=" only unknown")
  # check also the prepared test data for consistency
  checkTrue(all(is.na(reqResult$is_match)),
    msg=" only unknown")
  checkEquals(as.matrix(testResult), as.matrix(reqResult),
    msg=" only unknown")

  # 4. show non-matches and unknown
  testResult <- getPairs(rpairs, single.rows = FALSE,
    filter.match=c("nonmatch","unknown"))
  reqResult <- result2[2:6,]
  rownames(reqResult) <- 1:nrow(reqResult)
  checkTrue(all(is.na(testResult$is_match) | testResult$is_match==FALSE),
    msg=" non-match or unknown")
  # check also the prepared test data for consistency
  checkTrue(all(is.na(reqResult$is_match) | reqResult$is_match==FALSE),
    msg=" non-match or unknown")
  checkEquals(as.matrix(testResult), as.matrix(reqResult),
    msg=" non-match or unknown")

  # 5. show matches and unknown
  testResult <- getPairs(rpairs, single.rows = FALSE,
    filter.match=c("match","unknown"))
  reqResult <- result2[c(1,3,5,6),]
  rownames(reqResult) <- 1:nrow(reqResult)
  checkTrue(all(is.na(testResult$is_match) | testResult$is_match==TRUE),
    msg=" match or unknown")
  # check also the prepared test data for consistency
  checkTrue(all(is.na(reqResult$is_match) | reqResult$is_match==TRUE),
    msg=" match or unknown")
  checkEquals(as.matrix(testResult), as.matrix(reqResult),
    msg=" match or unknown")

  # 6. show matches and non-matches
  testResult <- getPairs(rpairs, single.rows = FALSE,
    filter.match=c("match","nonmatch"))
  reqResult <- result2[c(1,2,4),]
  rownames(reqResult) <- 1:nrow(reqResult)
  checkTrue(all(testResult$is_match==FALSE | testResult$is_match==TRUE),
    msg=" match or non-match")
  # check also the prepared test data for consistency
  checkTrue(all(reqResult$is_match==FALSE | reqResult$is_match==TRUE),
    msg=" match or non-match")
  checkEquals(as.matrix(testResult), as.matrix(reqResult),
    msg=" match or non-match")

  # check that empty result does not throw an error
  identity2 <- 1:4 # all records are different -> no matches
  rpairs <- RLBigDataDedup(data1, identity = identity2)
  testResult <- getPairs(rpairs, single.rows = FALSE, filter.match = "match")
  checkEquals(as.matrix(testResult), as.matrix(result2[NULL,]),
    msg = " empty result 1")
  testResult <- getPairs(rpairs, single.rows = FALSE,
    filter.match = c("match", "unknown"))
  checkEquals(as.matrix(testResult), as.matrix(result2[NULL,]),
    msg = " empty result 2")

  identity2 <- rep(NA, 4) # only "unknown" pairs
  rpairs <- RLBigDataDedup(data1, identity = identity2)
  testResult <- getPairs(rpairs, single.rows = FALSE, filter.match = "nonmatch")
  checkEquals(as.matrix(testResult), as.matrix(result2[NULL,]),
    msg = " empty result 3")
  testResult <- getPairs(rpairs, single.rows = FALSE,
    filter.match = c("match", "nonmatch"))
  checkEquals(as.matrix(testResult), as.matrix(result2[NULL,]),
    msg = " empty result 4")


  # check column names if database names are different
  colnames(data1)=c("fname.c1", "fname.c2", "lname.c1", "lname.c2", "by", "where", "select")
  rpairs <- RLBigDataDedup(data1, identity = identity2)
  testResult <- getPairs(rpairs)
  checkEquals(colnames(testResult), c("id.1", paste(colnames(data1), ".1", sep=""),
    "id.2", paste(colnames(data1), ".2", sep=""), "is_match"),
    msg = " check column names")
}


