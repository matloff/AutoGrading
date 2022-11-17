
# gives counts of A, B, C etc.

# arguments:

#    gradesfile: file, one line per student, course grade final field

tabcoursegrades <- function(gradesfile=T) {
   grades <- NULL
   for (student in readLines(gradesfile)) {
      # skip comment lines
      if (substr(student,1,1) == '#') next
      parts <- strsplit(student,split=" ")[[1]]
      grade <- parts[length(parts)]
      grades <- c(grades,grade)
   }
   table(grades)
}

