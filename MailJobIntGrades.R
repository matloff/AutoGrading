
# prep:
 
# 1.  Make sure roster file has a SINGLE field for student name, using
# '_' to separate surname and given name(s).

# 2.  Run $wwwm/AutoGrading/ExtractCols.py to extract the name and
# e-mail address fields, outputting to JobInterview.

# 3.  Enter grades.  Vi hint:  :map bp $a B+
 
# 4.  Run the below to send out.

mailjobint <- function() {
   recs <- readLines('JobInterview')
   for (rec in recs) {
      strout <- strsplit(rec,' ')[[1]]
      if (length(strout) == 3) {
         addr <- strout[2]
         grade <- strout[3]
         writeLines(grade,'tmpgrade')
         cmd <- 
            paste("mutt ",addr," -s ","'job interview grade'"," < ",'tmpgrade')
         print(cmd)
         system(cmd)
         system('/bin/rm tmpgrade')
      }
   }
}

