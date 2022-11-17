
# inputs my FinalGrades file, outputs course grades

# usage:  

#   Rscript QuickGradeReport.R

lns <- readLines('FinalGrades')
grades <- NULL
for (l in lns) { 
   words = strsplit(l,' ')[[1]]
   if (words[1] != '#')
         grades <- c(grades,words[length(words)])
}

table(grades)

