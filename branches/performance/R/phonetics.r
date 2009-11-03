# phonetic.r: functions for phonetic coding

pho_h <- function(str)
{
   if (is.factor(str))
     stop("pho_h does not work on factors")
   out <- .C("pho_h", as.character(str), ans=character(length(str)),length(str),
             PACKAGE="RecordLinkage")
   if (any(is.na(str)))
    out$ans[is.na(str)]=NA
   dim(out$ans)=dim(str)
   dimnames(out$ans)=dimnames(str)
   return(out$ans)
}

soundex <- function(str)
{
   if (is.factor(str))
     stop("soundex does not work on factors")
   out <- .C("soundex", as.character(str), ans=character(length(str)),length(str),
             PACKAGE="RecordLinkage")
   if (any(is.na(str)))
    out$ans[is.na(str)]=NA
   dim(out$ans)=dim(str)
   dimnames(out$ans)=dimnames(str)
   return(out$ans)
}
