
# for each student in the class e-mail list, sends the cmulative quiz
# records for that student; assumes there are directories Quiz1, Quiz2
# etc. right below where this code is run form, and that the grades are
# in a file beginning with 'Q'

# suffix 
mailquizrecs <- function(coursename,emaillist="EMailList",hasucdsuffix=TRUE) {
   addrs <- scan(emaillist,what="",sep="\n")
   for (addr in addrs) {
      if (!hasucdsuffix) {
         fulladdr <- paste(addr,"@ucdavis.edu",sep="")
      } else {
         fulladdr <- addr
         ## tmp <- strsplit(addr,"@")
         ## addr <- tmp[[1]][1]
      }
      coursecumul <- paste(coursename,'cumul quiz recs')
      muttcmd <- paste('mutt -s "',coursecumul,'" ',fulladdr,sep='')
      ## # guard against partial matches 
      ## addr <- paste(':',addr,' ',sep='')
      cmd <- paste("grep",addr,"Quiz*/Quiz*s |",muttcmd) 
      print(cmd)
      system(cmd)
   }
}
