
# script to semi-automate grading of ECS 256 quizzes

# IMPORTANT DIRECTIONS:

# This script will be invoked in a directory that has one subdirectory
# for each student, which is the form created by the CSIF handin
# program.  The script will descend into each one, grading the student
# there.

# The student's file must be named in the format email.tar, where
# 'email' consists of the student's UCD e-mail address (minus the
# "@ucdavis.edu"), separated by periods.  There must be no
# subdirectories archived within the file.

# Within the tar file, there must be the .pdf output, .tex input and
# other files required by the quzi.

# Penalties may be imposed for failure to follow directions.  

# The instructor places a file, RunCmdsGrpQuiz, in the current
# directory, of the following form.  There are 4 lines for each problem
# on the quiz:

#    problem number and points, e.g. #2 10 

#    source file name (1), no quotes, to be read into text editor

#    one line of run commands; for non-R code, these will rely on R's
#    system() call; be sure to include print()!

#    one line of cleanup commands, e.g. rm(x,y,x); usually blank, BUT
#    cannot be empty -- include at least one blank space

# Any line in RunCmdsGrpQuiz starting with ## is a comment, and is
# ignored; one can mention things like auxiliary files needed by the
# student code, provided by the instructor, or other special
# instructions, such as need to set global variables (see below)

# usage order:  
#
#    call grader() to grade the problems
#    call calcltrgrades() to compute the letter grades
#    call emailresults() to mail out the results
#
#    one of the output files will be named Quizn, containing the
#    full results, for Quiz n

#    important note: after each of the calls to grader() and
#    calcltrgrades(), the R variable output is saved to the file outfile
#    (in R object format); the latter can be copied to another machine
#    before calling emailresults()

# uses strsplit() to separate a single string s into substrings, defined
# by fields of one OR MORE blanks, including leading and trailing
# blanks; e.g. " abc  de f" is broken into c("abc","de","f"), with no ""
# components
blanksplit <- function(s) {
   tmp <- strsplit(s," ")[[1]]
   tmp[tmp != ""]
}

readcmdsfile <- function() {
   problemlist <<- list()
   lines <- scan(file="RunCmdsGrpQuiz",what="",sep="\n",quiet=T)
   # remove comments
   lines <- lines[substr(lines,1,2) != "##"]
   nlines <- length(lines)
   if (nlines %% 4 != 0) stop("nlines not a multiple of 4")
   nproblems <- nlines / 4
   problemlist$nproblems <<- nproblems
   # i-th element will be point total for the i-th problem
   problemlist$points <<- vector(mode="integer",length=nproblems)
   # i-th element will be a vector of source file names
   problemlist$src <<- vector(mode="character",length=nproblems)
   # i-th element will be a run string to assess student's work
   problemlist$run <<- vector(mode="character",length=nproblems)
   # i-th element will be a run string to do remove student's variables
   problemlist$cleanup <<- vector(mode="character",length=nproblems)
   for (i in 1:problemlist$nproblems) {
      l <- lines[4*i-3]
      problemlist$points[i] <<- strsplit(l," ")[[1]][2]
      problemlist$src[i] <<- lines[4*i-2]
      problemlist$run[i] <<- lines[4*i-1]
      problemlist$cleanup[i] <<- lines[4*i]
   }
}

# read students' source file for Problem i
openstudentsrcfile <- function(i) {
   src <- problemlist$src[i]
   srccmd <- paste("source('",src,"')",sep="")
   tmp <- try(docmd(srccmd))
   if (class(tmp) == "try-error") {
      print(paste("can't open ",src,"; note penalty"))
   }
}

# close students' source files for Problem i
# closestudentfiles <- function(i) {
#    for (src in problemlist$src[[i]]) {
#       xcmd <- paste("pkill -f ",src)
#       system(xcmd)
#    }
# }

# grade the i-th problem (counting subparts separately), using lines
# from the student answer file
gradestudentans <- function(i) {
   # read students' source file for Problem i
   openstudentsrcfile(i)  
   # try to run their code
   print("running student code")
   runcmd <- problemlist$run[i]
   print(paste("trying: ",runcmd))
   try(docmd(runcmd))
   # view student file
   readline(paste("hit Enter when ready to go to text editor"))
   src <- problemlist$src[i]
   cmd <- paste("vi ",src)
   system(cmd)
   # closestudentfiles(i)
   # assign score for this problem
   fullpts <- problemlist$points[i]
   resp <- readline(paste("pts out of ",fullpts, "? [empty means full pts] "))
   if (resp == "") fullpts else resp
}

# find the subdirectories, one for each student group
getgdirs <- function() {
   gdirs <- list.dirs(recursive=F)
   gdirs[gdirs != "."]
}

# unpack .tar file, find student name, check for proper structure, open
# PDF file
unpack <- function(pdf) {
   tmp <- list.files(pattern=".tar")
   # should be just one tar file, no further subdirectories
   if (length(tmp) != 1) {
      print(paste("problem in subdirectory",getwd())) 
      return(NULL)
   }
   # unpack .tar
   tarname <- tmp[1]
   cmd <- paste('system("tar xf ',tarname,'")',sep='')
   docmd(cmd)
   # get student name
   sname <- strsplit(tarname,'.',fixed=TRUE)[[1]][1]
   # open PDF
   pdfname <- list.files(pattern=".pdf")[1]
   pdfcmd <- paste(pdf,pdfname)
   cmd <- paste('system("',pdfcmd,'")')
   docmd(cmd)
   # return student e-mail address
   sname
}

# clean up the student's variables etc. so that the next student is not
# affected
cleanup <- function(i) {
   cmd <- problemlist$cleanup[i]
   if (length(cmd > 0)) docmd(cmd)
}

grader <- function() {
   quiznum <- readline("enter quiz number: ") 
   pdf <- readline("enter quoted name of PDF reader: ") 
   basedir <- getwd()
   gdirs <- getgdirs()  # get student subdirectory names
   output <<- NULL
   # go through all subdirectories, one per student
   for (gdir in gdirs) {  
      print(paste("entering directory", gdir))
      setwd(gdir)
      # try to open this student's .tar file, check for subdirectories
      # (not allowed), get student naumes etc.
      tmp <- try(
         {
         sname <- unpack(pdf)
         cat("\n\n","  now grading",sname,"\n")
         outputline <- NULL
         scores <- 
            readline('enter scores, sep by single space,  e.g. 25 45 0: ')
         total <- sum(Map(as.numeric,strsplit(scores,' '))[[1]])
         outputline <- scores
         penalty <- readline("penalty, e.g. late [0]:  ") 
         penalty <- if (penalty == "") 0 else as.integer(penalty)
         total <- total - penalty
         outputline <- paste(outputline,"penalty =",penalty)
         outputline <- paste(outputline,"total =",total)
         studentline <- paste(sname,outputline,sep=' ')
         print(studentline)
         output <- c(output,studentline)
         }
      )
      if (is.null(tmp)) {
         print(paste("unpack() had problems in: ", gdir))
         print("giving up; note penalty, grade separately later")
         next;
      }
      setwd(basedir)
   }
   repeat {
      if (readline("need to edit? ") == "y") {
         output <<- edit(output)
      } else break
   }
   cat("\n","results:","\n")
   save(output,file="outfile")
   for (i in 1:length(output)) {
      cat(output[i],"\n")
   }
}

calcltrgrades <- function() {
   load("outfile")  # load file named 'output', one line per student
   tmp <- readline("enter cutoffs (none for F), e.g. 95 A+ 85 A 70 B...: ")
   tmp <- strsplit(tmp," ")[[1]]
   inds <- 1:length(tmp)
   evens <- inds[inds %% 2 == 0]
   odds <- inds[inds %% 2 == 1]
   ltrgrades <- tmp[evens]
   cutoffs <- as.integer(tmp[odds])
   totcol <- length(blanksplit(output[1])) 
   # now go through all the students, assigning letters grades to each,
   # and appending the letter grade for a student to the student's line
   for (i in 1:length(output)) {
      total <- blanksplit(output[i])[totcol]
      total <- as.integer(total)
      ltrgrd <- num2ltr(total,cutoffs,ltrgrades)
      output[i] <- paste(output[i],ltrgrd)
   }
   # save to file for records
   save(output,file="outfile")
   write(output,file="QuizGrades")
   cat("\n","  letter grade results:","\n")
   for (i in 1:length(output)) {
      cat(output[i],"\n")
   }
   print("if not in office, upload outfile, GroupQuizGrades, RunCmdsGrpQuiz, .tex")
}

emailresults <- function() {
   load("outfile")
   for (l in output) {
      tmp <- strsplit(l," ")[[1]]
      emailaddr <- tmp[1]
      emailaddr <- paste(emailaddr,"@ucdavis.edu",sep="")
      cat(l,file="onestudent")
      tosend <- paste("mutt",emailaddr,"-s 'quiz results' < onestudent")
      system(tosend)
      # print(tosend)
      print(l)
      system("sleep 10")
      system("/bin/rm onestudent")
   }
}

# determines the letter gradeoff, based on the cutoffs cuts,lgs
num2ltr <- function(tot,cuts,lgs) {
   for (i in 1:length(cuts)) {
      if (tot >= cuts[i]) return(lgs[i])
   }
   return("F")
}

# do quoted command cmd
docmd <- function(cmd) {
   eval(parse(text=cmd))
}

