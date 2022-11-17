
# After running grader() in AutoGradeCourse.R, producing my own grades
# file, , use the code below to produce a .csv file to be uploaded to
# the Registrar.  First need to download .csv file (minus grades) from
# Registrar's grading site for my class.

# files:

#    FinalGrades:  my own private grades file
#    ForUpload.csv:  .csv grades file to be uploaded ot Registrar

makecsv <- function(regcsv) {

   # read my grades file twice, once as string-per-line and once  as df
   gradeslines <- readLines('FinalGrades')
   gradesdf <- read.table('FinalGrades',header=F)  
   gradecol <- ncol(gradesdf)  # what column is the grade in?
   # read Registrar's CSV file, also twice
   reglines <- readLines(regcsv)
   regdf <- read.csv(regcsv,header=T,stringsAsFactors=F)    
   # read my final class list, also downloaded from Registrar
   finalclasslist <- read.table('Enrollment/FinalClassList')
   
   myssns <- finalclasslist[,1]  # Soc Sec Numbers in my records
   grades <- file('ForUpload.csv','w')
   for (i in 1:nrow(regdf)) {
      # get Registrar Soc Sec Number to match to my file
      ssn <- regdf[i,4]
      ssn <- substr(ssn,2,10)
      ssn <- as.integer(ssn)
      # line number in my file for this SSN
      mymatch <- which(ssn == myssns)  
      grade <- gradesdf[mymatch,gradecol]
      # need to quote it
      grade <- paste('"',grade,'"',sep='')
      outputline <- paste(reglines[i+1],grade,sep='')
      cat(outputline,'\n',file=grades,append=T)
   }
}


