# utility; reads text file
readlns <- function(f) {
   scan(f,what="",sep="\n",quiet=T)
}

# extracts e-mail addresses from file f, 1 meaning first column, 2
# meaning last
getem <- function(f,col) {
   lns <- readlns(f)
   nl <- length(lns)
   email <- vector(length=nl)
   for (i in 1:nl) {
      parts <- strsplit(lns[i],split=" ")[[1]]
      if (col == 1) {
         email[i] <- parts[1]
      } else
         email[i] <- parts[length(parts)]
   }
   return(email)
}

# get e-mail addresses from class roster
rosterem <- getem("Roster",col=2)

# check all subdirectories
for (d in dir()) {
   fi <- file.info(d)
   if (fi$isdir) {
      if (d == "Midterm") {
         fname <- "Midterm/QuizMidtermGrades"
      } else 
         fname <- paste(d,'/',d,'Grades',sep="")
      cat("checking",fname,"\n")
      resultsem <- getem(fname,1)
      for (rem in rosterem) {
         if (!(rem %in% resultsem)) 
         cat(rem,"\n")
      }
   }
}
