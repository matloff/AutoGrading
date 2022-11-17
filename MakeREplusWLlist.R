
# forms class list

# reads in infilename,

# filters for the string " RE " (enrolled) or WL (waiting list)

# writes ID, name, class level, major, e-mail address to given file;
# name has underscores instead of blanks

makelist <- function(infilename,outfilename) {
   inlines <- readLines(infilename)
   for (iline in inlines) {
      # is this a line for an enrolled student?
      if (iline == "") next
      g <- 
      if (length(grep(" RE ",iline)) == 0 &&
          length(grep(" WL ",iline)) == 0) next
      # split by blanks
      ilparts <- strsplit(iline,split=" ")[[1]]
      ilparts <- ilparts[ilparts != "" & ilparts != "RE"
                                       & ilparts != "WL"]
      lil <- length(ilparts)
      # get student name
      tmp <- ilparts[3:(lil-3)]  # skip leading blanks, line number, ID
      nm <- Reduce(paste,tmp)
      # replace blanks by underscores
      nm <- gsub(" ","_",nm)
      tmp <- paste(ilparts[2],nm,Reduce(paste,ilparts[(lil-2):lil]))
      print(tmp)
      cat(tmp,"\n",file=outfilename,append=T)
   }
}

