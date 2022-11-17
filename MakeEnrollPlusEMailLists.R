
# forms class and e-mail lists (enrolled only) 

# reads in Registrar's class file, filters for the string " RE "
# (enrolled)

# outputs:

#    class list: ID, name, class level, major, e-mail address, 
#                where name has underscores instead of blanks
#                (file name is given in outfilename)

#    e-mail list

makelist <- function(infilename,outfilename) {
   inlines <- readLines(infilename)
   try (file.remove(c(outfilename,"EMailList")))
   for (iline in inlines) {
      # is this a line for an enrolled student?
      if (iline == "") next
      if (length(grep(" RE ",iline)) == 0) next
      # split by blanks
      ilparts <- strsplit(iline,split=" ")[[1]]
      ilparts <- ilparts[ilparts != "" & ilparts != "RE"]
      lil <- length(ilparts)
      # get student name
      tmp <- ilparts[3:(lil-3)]  # skip leading blanks, line number, ID
      nm <- Reduce(paste,tmp)
      # replace blanks by underscores
      nm <- gsub(" ","_",nm)
      tmp <- paste(ilparts[2],nm,Reduce(paste,ilparts[(lil-2):lil]))
      print(tmp)
      cat(tmp,"\n",file=outfilename,append=T)
      eaddress <- ilparts[lil]
      cat(eaddress,"\n",sep="",file="EMailList",append=T)
   }
}

