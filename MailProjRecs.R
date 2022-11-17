
# e-mails the project grades; assumes:

# 1. The file EMailList in the current directory contains official UCD
# e-mail addresses, with or without the @ucdavis.edu suffix.
# 
# 2.  The file Projects/Grades has the project grades, indexed by student
# e-mail address (without suffix).

mailprojrecs <- function(course,hasucdsuffix=FALSE) {
   addrs <- scan("EMailList",what="",sep="\n")
   for (addr in addrs) {
      if (!hasucdsuffix) {
         fulladdr <- paste(addr,"@ucdavis.edu",sep="")
      } else {
         fulladdr <- addr
         tmp <- strsplit(addr,"@")
         addr <- tmp[[1]][1]
      }
      tmp <- paste("grep",addr,"Project/Grades")
      tmp <- system(tmp,intern=TRUE)
      cat(tmp,file="onestudent")
      subject <- paste(course,'project grade',sep=" ")
      tosend <- paste("mutt",fulladdr,"-s '", subject, "' < onestudent")
      print(tosend)
      system(tosend)
   }
}
