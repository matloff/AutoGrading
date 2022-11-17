
# forms class list, writing only relevant fields, and forming names with
# underscores, e.g. John_Smith   

# reads in roster that has been obtained from

#  https://sisr.ucdavis.edu/secure/reporting/reports/Report.cfm?reportName=StudentClassRoster

# no longer filters for the string "RE " (enrolled)

# writes ID, name, class level, major, e-mail address to given file

makelist <- function(infilename,outfilename) {
   inlines <- readLines(infilename)
   for (iline in inlines) {
      # is this a line for an enrolled student?
      if (iline == "") next
      # split by TABs
      ilparts <- strsplit(iline,split="\t")[[1]]
      # if (!'RE ' %in% ilparts) next
      ilparts <- ilparts[ilparts != "" & ilparts != "RE "]
      ID <- ilparts[2]
      ### if (ID == '912133793') browser()
      whichem <- length(ilparts)
      nm <- ilparts[3:4]
      nm[1] <- gsub(' ','.',nm[1])
      nm[2] <- gsub(' ','.',nm[2])
      nm <- paste(nm,collapse='.')
      classlvl <- ilparts[whichem-3]
      major <- ilparts[whichem-2]
      email <- ilparts[whichem]
      tmp <- paste(ID,nm,classlvl,major,email,sep=' ')
      print(tmp)
      cat(tmp,"\n",file=outfilename,append=T)
   }
}

