
# arguments:

#    emailfile: file of e-mail addresses, WITHOUT UCD suffix
#    gradesfile: file, one line per student, indexed by addresses in emailfile
#    nomail: if TRUE, trial run

# emailfile must NOT have UCD suffix
mailcoursegrades <- function(emailfile,gradesfile,nomail=T) {
   addrs <- scan(emailfile,what="",sep="\n")
   for (addr in addrs) {
       fulladdr <- paste(addr,"@ucdavis.edu",sep="")
       cmd <- paste("grep",addr,gradesfile)
       cmd <- paste(cmd, "| mail", fulladdr)
       print(cmd)
       if (!nomail) system(cmd)
   }
}
