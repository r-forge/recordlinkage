#
##### Tests for compare.linkage ####
#
## test all kinds of illegal input
#test.compare.linkage.exceptions <- function()
#{
#
#
#  # illegal dataset
#  checkException(compare.linkage(data2, "andreas")) # wrong data type
#  checkException(compare.linkage("andreas", data2)) # wrong data type
#
#
#  # illegal blocking definition
##  checkException(compare.linkage(data2, data3, blockfld="fname_c1")) # wrong type
##  checkException(compare.linkage(data2, data3, blockfld=TRUE)) # wrong type/value
#  checkException(compare.linkage(data2, data3, blockfld=list(1,list(4,6)))) # nested list
##  checkException(compare.linkage(data2, data3, blockfld=-3)) # negative index
##  checkException(compare.linkage(data2, data3, blockfld=0))
#  
#  # illegal phonetic definition
#  checkException(compare.linkage(data2, data3, phonetic="fname_c1")) # wrong type
#  checkException(compare.linkage(data2, data3, phonetic=list(1,4))) # list not okay
##  checkException(compare.linkage(data2, data3, phonetic=-3)) # negative index
##  checkException(compare.linkage(data2, data3, phonetic=0))
#    
#  # illegal phonetic function
#  checkException(compare.linkage(data2, data3, phonetic=TRUE, phonfun=5)) # not a function
#  checkException(compare.linkage(data2, data3, phonetic=TRUE, phonfun="jarowinkler")) # not a function
#  checkException(compare.linkage(data2, data3, phonetic=TRUE, 
#    phonfun=list(pho_h, soundex))) # neither list...
#  checkException(compare.linkage(data2, data3, phonetic=TRUE, 
#    phonfun=c(pho_h, soundex))) # ...nor vector makes sense
#  # how to test if function returns the right thing?
#    
#  # illegal string comparator definition
#  checkException(compare.linkage(data2, data3, strcmp="fname_c1")) # wrong type
#  checkException(compare.linkage(data2, data3, strcmp=list(1,4))) # list not okay
##  checkException(compare.linkage(data2, data3, strcmp=-3)) # negative index
##  checkException(compare.linkage(data2, data3, strcmp=0))
#
#  # illegal string comparison function
#  checkException(compare.linkage(data2, data3, strcmp=TRUE, phonfun=5)) # not a function
#  checkException(compare.linkage(data2, data3, strcmp=TRUE, phonfun="jarowinkler")) # not a function
#  checkException(compare.linkage(data2, data3, strcmp=TRUE, 
#    phonfun=list(jarowinkler, levenshteinSim))) # neither list...
#  checkException(compare.linkage(data2, data3, strcmp=TRUE, 
#    phonfun=c(jarowinkler, levenshteinSim))) # ...nor vector makes sense
#  # how to test if function returns the right thing?
#
#  # illegal exclude field definition    
#  checkException(compare.linkage(data2, data3, exclude=c(4,10))) # out of bounds
#  checkException(compare.linkage(data2, data3, exclude="fname_c1")) # wrong type
#  checkException(compare.linkage(data2, data3, exlude=list(1,4))) # list not okay
##  checkException(compare.linkage(data2, data3, exclude=-3)) # negative index
##  checkException(compare.linkage(data2, data3, exclude=0))
#
#  # illegal identity vector
#  checkException(compare.linkage(data2, data3,identity=as.list(identity1)))
#
#  # illegal type for n_match and n_non_match
#  checkException(compare.linkage(data2, data3, identity=identity1, n_match="1",
#    n_non_match=2))
#  checkException(compare.linkage(data2, data3, identity=identity1, n_match=1,
#    n_non_match="2"))
#  checkException(compare.linkage(data2, data3, identity=identity1, n_match=TRUE,
#    n_non_match="2"))
#  checkException(compare.linkage(data2, data3, identity=identity1, n_match=1,
#    n_non_match=FALSE))
#  checkException(compare.linkage(data2, data3, identity=identity1, n_match=factor(1),
#    n_non_match=2))
#  checkException(compare.linkage(data2, data3, identity=identity1, n_match=1,
#    n_non_match=factor(2)))
#     
#  # combinations of arguments that cause an error
#
#  # non-matching data sets
#  checkException(compare.linkage(data2, data3[-2]))
#  
#  # dataset and blocking definition:
#  checkException(compare.linkage(data2, data3, blockfld=c(4,10))) # out of bounds
#  checkException(compare.linkage(data2, data3, blockfld=c("fname_c1","lname"))) # non-existing column
#
#  # dataset and phonetic:
#  checkException(compare.linkage(data2, data3, phonetic=c(4,10))) # out of bounds
#
#  # dataset and strcmp:
#  checkException(compare.linkage(data2, data3, strcmp=c(4,10))) # out of bounds
#
#  # dataset and exclude
#  checkException(compare.linkage(data2, data3, exclude=c(1,10))) # out of bounds
#  
#  # dataset and identity vector (illegal length)
#  checkException(compare.linkage(data2, data3, identity1=1:2, identity2=identity3))
#  checkException(compare.linkage(data2, data3, identity1=identity2, identity2=1:10))
#
#  # dataset and n_match / n_non_match: see below
#  
#  # blockfld: no conflicts with other args
#  
#  # phonetic and strcmp
#  # error: string comparator and phonetic code for same column
#  # first set warnings to errors
#  oldWarn <- getOption("warn")
#  options(warn=2)
#  checkException(compare.linkage(data2, data3, strcmp=1, phonetic=1))  # single column
#  checkException(compare.linkage(data2, data3, strcmp=1:3, phonetic=3)) # one overlap
#  checkException(compare.linkage(data2, data3, strcmp=3, phonetic=1:3))  
#  checkException(compare.linkage(data2, data3, strcmp=1:3, phonetic=2:4)) # several overlaps 
#  checkException(compare.linkage(data2, data3, strcmp=1:3, phonetic=2:4))  
#  checkException(compare.linkage(data2, data3, strcmp=TRUE, phonetic=TRUE))  
#  checkException(compare.linkage(data2, data3, strcmp=1, phonetic=TRUE))  
#  checkException(compare.linkage(data2, data3, strcmp=TRUE, phonetic=1))  
#  # reset warning handling
#  options(warn=oldWarn)
#  
#  # strcmp: no conflicts with other args
#  # exclude: no conflicts with other args
#  # ...
#  
#}
#
#
## test 'normal' behaviour including errors that occur later during execution
#test.compare.linkage <- function()
#{
#
#
#  # no blocking etc.
#  testResult=compare.linkage(data2,data3) # default case: no blocking whatsoever
#  reqResult=read.table("result8.compare.txt",sep=",",colClasses="double",
#    header=TRUE)
#  checkEquals(testResult$pairs, reqResult, msg=" (no blocking or other options)")
#
#  # check other components only once
#  checkEquals(testResult$type, "linkage", msg=" check type component)")
#  checkEquals(testResult$data1, data2, msg=" (check data1 component)")
#  checkEquals(testResult$data2, data3, msg=" (check data2 component)")
#  checkEqualsNumeric(testResult$frequencies, frequencies1, tolerance=1e-6,
#    msg=" (check frequencies)")
#
#  # same case with usage of identity vector:
#  testResult=compare.linkage(data2, data3, identity1=identity2,
#    identity2=identity3)
#  reqResult=read.table("result9.compare.txt",sep=",",colClasses="double",
#    header=TRUE)
#  checkEquals(testResult$pairs, reqResult, msg=" (check identity vector)")
#
#  # now use blocking on last name  
#  # also tests behaviour for result with only one pair
#  testResult=compare.linkage(data2, data3, identity1=identity2, 
#    identity2=identity3, blockfld=3)
#  reqResult=read.table("result10.compare.txt",sep=",",colClasses="double",
#    header=TRUE)
#  checkEquals(testResult$pairs, reqResult, msg=" (blocking on one component)")
#  # repeat with textual column id 
#  testResult=compare.linkage(data2, data3, identity1=identity2, 
#    identity2=identity3, blockfld="lname_c1")
#  checkEquals(testResult$pairs, reqResult, 
#    msg=" (blocking on one component, text id)")
#  
#   
#  # same blocking with phonetic coding yields more results
#  testResult=compare.linkage(data2, data3, identity1=identity2, 
#    identity2=identity3, blockfld=3, phonetic=1:4)
#  reqResult=read.table("result11.compare.txt",sep=",",colClasses="double",
#    header=TRUE)
#  checkEquals(testResult$pairs, reqResult, msg=" (blocking with phonetic code")
#
#  # blocking on two components
#  testResult=compare.linkage(data2, data3, identity1=identity2, 
#    identity2=identity3, blockfld=c(5,6))
#  reqResult=read.table("result12.compare.txt",sep=",",colClasses="double",
#    header=TRUE)
#  checkEquals(testResult$pairs, reqResult, msg=" (blocking with two components")
#  # repeat with textual column id 
#  testResult=compare.linkage(data2, data3, identity1=identity2, 
#    identity2=identity3, blockfld=c("by", "bm"))
#  checkEquals(testResult$pairs, reqResult, 
#    msg=" (blocking with two components, text id)")
#
#  # combine blocking on data components with blocking on last name
#  # (disjunction)
#  testResult=compare.linkage(data2, data3, identity1=identity2, 
#    identity2=identity3, blockfld=list(3,c(5,6)))
#  reqResult=read.table("result13.compare.txt",sep=",",colClasses="double",
#    header=TRUE)
#  checkEquals(testResult$pairs, reqResult, msg=" (combined blocking criteria")
#  # repeat with textual column id 
#  testResult=compare.linkage(data2, data3, identity1=identity2, 
#    identity2=identity3,  blockfld=list("lname_c1",c("by", "bm")))
#  checkEquals(testResult$pairs, reqResult, msg=" (combined blocking criteria")
#
#  # too restrictive blocking (i.e. no resulting pairs) should fail)
#  
#    
#  checkException(compare.linkage(data2, data3, blockfld=1:4), 
#    msg=" (no record pairs generated)")
#  
#  # exclude columns, still allow blocking and phonetic code
#  testResult=compare.linkage(data2, data3, identity1=identity2, 
#    identity2=identity3,  blockfld=3, phonetic=1:4, exclude=c(3,4))
#  reqResult=read.table("result14.compare.txt",sep=",",colClasses="double",
#    header=TRUE)
#  checkEquals(testResult$pairs, reqResult, msg=" (exclude columns)")
#  # check if frequencies are based on used fields
#  checkEqualsNumeric(testResult$frequencies, frequencies1[-c(3,4)],
#    tolerance = 1e-6, msg = " (check frequencies with excluded attributes)")
#  checkEquals(names(testResult$frequencies), 
#    colnames(testResult$pairs)[-c(1,2,ncol(testResult$pairs))],
#    msg = "check names of frequencies with excluded attributes")
#  
#
#  # use string comparator for all fields
#  # actual values are tested seperately (tests for strcmp.r)
#  testResult=compare.linkage(data2, data3, identity1=identity2, 
#    identity2=identity3, blockfld=3, strcmp=TRUE, strcmpfun=jarowinkler)
#  reqResult=c(1,1,jarowinkler( c("FRANK",NA,"MUELLER",NA,1967,9,27), 
#    c("MARTIN",NA,"MUELLER",NA,1950,2,4)),0)
#  checkEquals(as.double(testResult$pairs),reqResult, msg=" (jarowinkler, all fields)")
#  
#  # string comparator for individual fields
#  testResult=compare.linkage(data2, data3, identity1=identity2, 
#    identity2=identity3, blockfld=3, strcmp=1:4, strcmpfun=jarowinkler)
#  reqResult=c(1,1,jarowinkler( c("FRANK",NA,"MUELLER",NA), 
#    c("MARTIN",NA,"MUELLER",NA)),0,0,0,0)
#  checkEquals(as.double(testResult$pairs),reqResult, msg=" (jarowinkler, selected fields)")
#    
#  # same tests for levenshteinSim
#  testResult=compare.linkage(data2, data3, identity1=identity2, 
#    identity2=identity3, blockfld=3, strcmp=TRUE, strcmpfun=levenshteinSim)
#  reqResult=c(1,1,levenshteinSim( c("FRANK",NA,"MUELLER",NA,1967,9,27), 
#    c("MARTIN",NA,"MUELLER",NA,1950,2,4)),0)
#  checkEquals(as.double(testResult$pairs),reqResult, msg=" (levenshteinSim, all fields)")
#
#    reqResult=c(1,1,levenshteinSim( c("FRANK",NA,"MUELLER",NA), 
#    c("MARTIN",NA,"MUELLER",NA)),0,0,0,0)
#  testResult=compare.linkage(data2, data3, identity1=identity2, 
#    identity2=identity3, blockfld=3, strcmp=1:4, strcmpfun=levenshteinSim)
#  checkEquals(as.double(testResult$pairs),reqResult, msg=" (levenshteinSim, selected fields)")
#
#
#  
#  # random sampling
#  # draw 1 match and 3 non-matches, check if desired numbers are met
#  testResult=compare.linkage(data2, data3, identity1=identity2, 
#    identity2=identity3, n_match=1, n_non_match=3)
#  n_match=sum(testResult$pairs$is_match==1)
#  n_non_match=sum(testResult$pairs$is_match==0)
#  checkEquals(c(n_match, n_non_match), c(1,3), msg=" (random sampling)")
#
#  # if not enough matches or non-matches are available, limit to the maximum
#  # number
#  testResult=compare.linkage(data2, data3, identity1=identity2, 
#    identity2=identity3, n_match=2, n_non_match=3)
#  n_match=sum(testResult$pairs$is_match==1)
#  n_non_match=sum(testResult$pairs$is_match==0)
#  checkEquals(c(n_match, n_non_match), c(1,3), 
#    msg=" (random sampling, not enough matches)")
#
#  # same for not enough non-matches
#  testResult=compare.linkage(data2, data3, identity1=identity2, 
#    identity2=identity3, n_match=1, n_non_match=6)
#  n_match=sum(testResult$pairs$is_match==1)
#  n_non_match=sum(testResult$pairs$is_match==0)
#  checkEquals(c(n_match, n_non_match), c(1,5), 
#    msg=" (random sampling, not enough non-matches)")
#  # check if equal with all pairs
#  ids=as.matrix(testResult$pairs[,1:2])
#  all_ids=as.matrix(merge(data.frame(id1=1:3), data.frame(id2=1:2), all=TRUE))
#  checkEqualsNumeric(ids[order(ids[,1], ids[,2]),], 
#    all_ids[order(all_ids[,1], all_ids[,2]),],
#      msg=" (random sampling, not enough non-matches)")
#
#  # not enough matches AND non-matches
#  testResult=compare.linkage(data2, data3, identity1=identity2, 
#    identity2=identity3, n_match=4, n_non_match=13)
#  n_match=sum(testResult$pairs$is_match==1)
#  n_non_match=sum(testResult$pairs$is_match==0)
#  checkEquals(c(n_match, n_non_match), c(1,5), 
#    msg=" (random sampling, not enough matches and non-matches)")
#  # check if equal with all pairs
#  ids=as.matrix(testResult$pairs[,1:2])
#  checkEqualsNumeric(ids[order(ids[,1], ids[,2]),], 
#    all_ids[order(all_ids[,1], all_ids[,2]),],
#    msg=" (random sampling, not enough matches and non-matches)")
#
#
#  # check if warnings are issued in this case  
#  oldWarn <- getOption("warn")
#  options(warn=2)
#  checkException(compare.linkage(data2, data3, identity1=identity2, 
#    identity2=identity3, n_match=2, n_non_match=3),
#    msg = " (warning for not enough matches)")
#  checkException(compare.linkage(data2, data3, identity1=identity2, 
#    identity2=identity3, n_match=1, n_non_match=6),
#    msg = " (warning for not enough non-matches)")  
#  checkException(compare.linkage(data2, data3, identity1=identity2, 
#    identity2=identity3, n_match=4, n_non_match=13),
#    msg = " (warning for not enough matches and non-matches)")
#  options(warn=oldWarn)
#
#  ### various other checks (e.g. for new found bugs) ###
#  
#  # check if column names are created if not supplied
#  data2noNames=as.matrix(data2)
#  colnames(data2noNames)=NULL
#  testResult=compare.linkage(data2noNames, data3)
#  resultNames=colnames(testResult$pairs)
#  checkTrue(!any(is.na(resultNames)), msg=" (check column names)") # no NAs
#  checkEquals(anyDuplicated(resultNames),0, 
#    msg=" (check column names)")            # uniqueness
#  checkEquals(resultNames[1:2],c("id1","id2"), 
#    msg=" (check column names)")            # correct id names
#  checkEquals(tail(resultNames,1),"is_match",
#    msg=" (check column names)")            # correct name for match status
#
#  # test for a bug that occured when "NA" was part of a name, check also
#  # for "NULL"
#  data5 <- read.table("data5.compare.txt", sep=",", na.strings="",header=TRUE)
#  data6 <- read.table("data6.compare.txt", sep=",", na.strings="",header=TRUE)
#  testResult <- compare.linkage(data5, data6, blockfld=3, phonetic=3)
#  reqResult <- read.table("resultNA2.compare.txt",sep=",",colClasses="double",
#    header=TRUE)
#  checkEquals(testResult$pairs, reqResult, msg=" (check reserved words in data)")
#}
#
## names of datasets differ?
#
#
#test.getPatternCounts <- function()
#{
#  # Test für Dedup-Objekt
#  object <- RLBigDataDedup(data1) # default case: no blocking whatsoever
#  result1=read.table("result1.getPatternCounts.txt")
#  # Check only numeric equality. Reason: result1 is read as a data frame with
#  # one column, which is not easily convertible to a vector with names
#  checkEqualsNumeric(getPatternCounts(object), result1[[1]])
#  checkEqualsNumeric(getPatternCounts(object,n=1), result1[[1]])
#  checkEqualsNumeric(getPatternCounts(object,n=4), result1[[1]])
#}
#