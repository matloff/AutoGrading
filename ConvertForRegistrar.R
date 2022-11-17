
# download .csv file from Registrar's grade site, e.g.

#    read.csv('201910_ECS_132_A01-A02_final_grades.csv')  

# with a typical row being 

#   "202001","ECS 145 - 001","77380","'916217178","Jayaram","Atul","",

# note that that last field, "", is empty because that is where the
# student's grade will go; note too that the main index is the student ID

# then read in my own grades file, hard coded as FinalGrades; for the
# same reason as in the Registrar's file, all it really needs is student
# ID and the grade, in the proper cols

# finally, for each line in the Registrar's file, look for the
# matching student ID in my file; if missing, print a message but move
# on; for each student found, replace the Registrar's empty string grade
# field by the actual grade; output the updated Registrar file, for
# uploading

convertToReg <- function(registarFile,
      regSID=4,regGradeCol=8, 
      meGradeCol=24) 
{
   ucd <- read.csv(registarFile)
   me <- readLines('FinalGrades')
   for (i in 1:nrow(ucd)) {
      if (is.na(ucd[i,regGradeCol])) ucd[i,regGradeCol] <- ""
      sid <- ucd[i,regSID]  # student ID
      sid <- substr(sid,2,9)  # rm extra '
      grout <- grep(sid,me)
      if (length(grout) == 0) {
         cat('no record for ',sid,'\n')
         next
      }
      mylinenum <- grout
      tmp <- strsplit(me[mylinenum],' ')[[1]][meGradeCol]
      ucd[i,regGradeCol] <- tmp
   }
   write.csv(ucd,file='ucd.csv',row.names=F)
}

