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

  # first, conduct tests with single.rows=TRUE (easier to check)
  #
  # column classes are chosen so that text fields are factors and number fields
  # (including date of birth) are integer
  #
  # but: column classes are ignored as it is difficult to enforce
  # the same format as in the data (conversion by SQLite engine)

  # get all pairs without restriction
  reqResult <- read.table("result2.getPairs.txt",sep=",",header=TRUE,
    colClasses = c(rep(c("integer", rep("factor", 4), rep("integer", 3)),2),
    "logical"))
  testResult <- getPairs(rpairs, single.rows = TRUE)
  checkEquals(as.matrix(testResult), as.matrix(reqResult),
    check.attributes=FALSE, msg = " all pairs on single rows")

  # tests for argument filter.match
  #
  # The table of expected results (reqTable) holds pairs with the following
  # status: T F NA F NA NA. In the following, the slice with "allowed"
  # outcome is selected and compared to the output of getPairs()
  result2 <- reqResult
  
  # 1. show only non-matches
  testResult <- getPairs(rpairs, single.rows = TRUE, filter.match="nonmatch")
  reqResult <- result2[c(2,4),]
  rownames(reqResult) <- 1:nrow(reqResult)
  checkTrue(all(testResult$is_match==FALSE),
    msg=" only non-matches")
  # check also the prepared test data for consistency
  checkTrue(all(reqResult$is_match==FALSE),
    msg=" only non-matches")
  checkEquals(as.matrix(testResult), as.matrix(reqResult),
    check.attributes=FALSE, msg=" only non-matches")

  # 2. show only matches
  testResult <- getPairs(rpairs, single.rows = TRUE, filter.match="match")
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
  testResult <- getPairs(rpairs, single.rows = TRUE, filter.match="unknown")
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
  testResult <- getPairs(rpairs, single.rows = TRUE,
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
  testResult <- getPairs(rpairs, single.rows = TRUE,
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
  testResult <- getPairs(rpairs, single.rows = TRUE,
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

  # check argument withWeight
  # implicit inclusion of weights
  rpairs <- epiWeights(rpairs)
  testResult <- getPairs(rpairs, single.rows=TRUE)
  # now the result should have a column "Weight" with weights in a suitable
  # range (in the case of epilink, real numbers between 0 and 1, inclusive)
  checkTrue(all(testResult$Weight <= 1 && testResult$Weight >=0),
    msg = "Check that valid weights are in the result table if implicitly requested")

  # now include weights explicitly
  testResult <- getPairs(rpairs, single.rows=TRUE, withWeight=TRUE)
  # now the result should have a column "Weight" with weights in a suitable
  # range (in the case of epilink, real numbers between 0 and 1, inclusive)
  checkTrue(all(testResult$Weight <= 1 && testResult$Weight >=0),
    msg = "Check that valid weights are in the result table if explicitly requested")

  # now exclude weights explicitly
  testResult <- getPairs(rpairs, single.rows=TRUE, withWeight=FALSE)
  # now the result should have a column "Weight" with weights in a suitable
  # range (in the case of epilink, real numbers between 0 and 1, inclusive)
  checkTrue(is.null(testResult$Weight),
    msg = "Check that no weights are in the result table if explicitly requested")


  # check filtering by weight range
  # pick a weight that lies "in the middle" of the data with the restrictions
  # that it is > 0 and corresponds to an existing value
  W <- unique(sort(getPairs(rpairs, single.rows=TRUE)$Weight))
  medianW <- W[ceiling(length(W)/2)]

  # with min.weight
  testResult <- getPairs(rpairs, single.rows=TRUE, min.weight=medianW)
  checkTrue(all(testResult$Weight >= medianW))
  
  # with max.weight
  testResult <- getPairs(rpairs, single.rows=TRUE, max.weight=medianW)
  checkTrue(all(testResult$Weight <= medianW))

  # with both. Now, pick a value between two weights to test exclusion
  minW <- mean(W[1:2])
  maxW <- mean(tail(W, 2))
  testResult <- getPairs(rpairs, single.rows=TRUE, max.weight=maxW,
    min.weight=minW)
  checkTrue(all(testResult$Weight >= minW & testResult$Weight <= maxW))


  # combination of weight range and matching status
  
  # this should give only one pair with the test data
  testResult <- getPairs(rpairs, single.rows=TRUE, max.weight=0.2,
    min.weight=0.1, filter.match="unknown")
  checkEquals(nrow(testResult), 1, msg="Check combination of weight range and matching status")
  checkTrue(all(is.na(testResult$is_match) & testResult$Weight >= 0.1 & testResult$Weight <= 0.2 ),
    msg="Check combination of weight range and matching status")


  # Check result for single.rows=TRUE. This is only a reformatted result,
  # so one check for this case should be enough
  # the following code circumvents the problem that as.matrix.data.frame
  # right-aligns numeric columns by inserting spaces
  testResult <- gsub(" ", "", as.matrix(getPairs(rpairs, withWeight=FALSE)))
  reqResult <- read.table("result1.getPairs.txt",sep=",",header=TRUE, colClasses="factor")
  reqResult <- gsub(" ", "", as.matrix(reqResult))
  checkEquals(as.matrix(testResult), reqResult)

  # check that empty result does not throw an error
  identity2 <- 1:4 # all records are different -> no matches
  rpairs <- RLBigDataDedup(data1, identity = identity2)
  testResult <- getPairs(rpairs, single.rows = TRUE, filter.match = "match")
  checkEquals(nrow(testResult), 0,
    msg = " empty result 1")
  checkEquals(colnames(testResult), colnames(result2), msg=" emtpy result 1")

  testResult <- getPairs(rpairs, single.rows = FALSE,
    filter.match = c("match", "unknown"))
  checkEquals(nrow(testResult), 0,
    msg = " empty result 2")
  checkEquals(colnames(testResult), colnames(reqResult), msg=" emtpy result 1")


  identity2 <- rep(NA, 4) # only "unknown" pairs
  rpairs <- RLBigDataDedup(data1, identity = identity2)
  testResult <- getPairs(rpairs, single.rows = FALSE, filter.match = "nonmatch")
  checkEquals(nrow(testResult), 0,
    msg = " empty result 3")
  checkEquals(colnames(testResult), colnames(reqResult), msg=" emtpy result 3")

  testResult <- getPairs(rpairs, single.rows = FALSE,
    filter.match = c("match", "nonmatch"))
  checkEquals(nrow(testResult), 0,
    msg = " empty result 4")
  checkEquals(colnames(testResult), colnames(reqResult), msg=" emtpy result 4")

  
  # check column names if database names are different
  colnames(data1)=c("fname.c1", "fname.c2", "lname.c1", "lname.c2", "by", "where", "select")
  rpairs <- RLBigDataDedup(data1)
  testResult <- getPairs(rpairs, single.row=TRUE)
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


