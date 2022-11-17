
# reads in file of e-mail addresses, one per line, from EMailList;
# searches for each address in the file HomeworkGrades; e-mails the
# results

mailhwkrecs <- function() {
   addrs <- scan("EMailList",what="",sep="\n")
   for (addr in addrs) {
       fulladdr <- paste(addr,"@ucdavis.edu",sep="")
       cmd <- paste("grep",addr,"HomeworkGrades | mail", fulladdr)
       print(cmd)
       system(cmd)
   }
}

