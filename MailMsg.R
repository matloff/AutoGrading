
# arguments:
# 
#    msg:  name of file containing body of the message to be sent
#    emaillist: name of file containing the full e-mail addresses to be sent to
#    subdoublequote:  Subject of msg; IMPORTANT NOTE:  should have quotes 
#                     within quotes, e.g. "'a bc'"
#     nomail: if TRUE, just do test run

mailmsg <- function(msg,emaillist,subdoublequote,nomail=F) {
   addrs <- scan(emaillist,what="",sep="\n")
   for (addr in addrs) {
       # cmd <- paste("mail ",addr," -s ",subdoublequote," < ",msg)
       cmd <- paste("mutt ",addr," -s ",subdoublequote," < ",msg)
       print(cmd)
       if (!nomail) system(cmd)
       Sys.sleep(1)
   }
}

