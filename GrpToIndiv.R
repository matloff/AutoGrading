# inputs grade file for groups, replaces each line by individual scores;
# for instance

#    smith.jones 0/20 40/80 total = 40 C+
# 
# becomes
# 
#    smith 0/20 40/80 total = 40 C+
#    jones 0/20 40/80 total = 40 C+

grptoindiv <- function(quizfile) {
    lns <- readLines(quizfile)
    outtext <- NULL
    for (l in lns) {
       team <- strsplit(l,' ')[[1]][1]
       lteam <- nchar(team)
       scores <- substr(l,lteam+1,nchar(l))
       tnames <- strsplit(team,'.',fixed=TRUE)
       for (tn in tnames) {
          oneperson <- paste(tn,'@ucdavis.edu',scores,sep='')
          outtext <- c(outtext,oneperson)
       }
    }
    writeLines(outtext,'outfile')
}
