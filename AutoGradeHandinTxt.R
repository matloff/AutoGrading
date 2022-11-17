
# Script to semi-automate grading.

# Assumes Mac or Linux machine, though could be easily modified for
# Windows.  The only modification would be using a .zip file instead of
# .tar, with a command to unzip.

# IMPORTANT DIRECTIONS:

# This script will be invoked in a directory, called the "top" directory
# here, that has one subdirectory for each student.  The script will
# descend into each subdirectory, grading the student there.  Grading is
# invoked by calling grader().

# Each such subdirectory is assumed to contain the student submission in
# a single file, either .txt or .tar.

# By default, the file names are in the form x.y, where x is a student
# ID in the form of an e-mail address (see below).  The suffix y is .txt
# by default, but could be whatever the instructor needs, say .c, .tar
# etc.

# The student's e-mail address is presumed to be lacking the domain
# name, e.g. jdlee rather than jdlee@ucdavis.edu.  The domain name can
# be specified in the argument 'emaildom' in grader(), or can be set
# there to '' if the address is already complete.

# It is also possible to grade student groups working collaboratively.
# In that case, student file name is of the form a.b.c.y, where 'a' etc.
# are the students' e-mail addresses.  This situation is indicated by
# setting 'group = TRUE' in the call to grader().

# The instructor places a file, Answersn For quiz n, in the top
# directory to indicate point totals and answers (or outlines of
# answers) for each question.  Each problem in this file begins with a
# line consisting of %, the problem number and the point total.  The
# lines that follow form the answer to the problem.  For example:

#   %1 15
#   The butler
#   did it.

# Optionally, the point total in the % line is followed by an R command
# to be executed for that problem, which could be pure R or 
# a call to R's system() function, thus executing a shell command.  

# For example, the instructor may ask the students to
# write a function, f(), and test it by calling f(8). The entry in
# Answersn may look like this:

#   %3 25 f(8)
#   Instructor would write f() in here, either 
#   in full or in outline.

# Here is a more elaborate one:

#   %1 50 source('Problem1.R'); print(try(dcolmeansm(cls))); readline('hit Enter '); edit(dcolmeansm); rm(dcolmeansm,envir=.GlobalEnv) 
#   see Quiz8.R

# Note the use of rm(), to ensure that work left by one student is not
# used by the next.

# The default scenario is that in which there is jsut one student answer
# file, with y = .txt, in which case it is assumed that there is no
# command.

# If no command is specified, the action taken is to just call R's
# edit() function on the student file.  The instructor can of course add
# an edit() call in a command, and this is recommended, e.g..

#   %3 25 f(8); readline('hit Enter '); edit('f.R')
#   Instructor would write f() in here, either 
#   in full or in outline.

# In many cases, the instructor will set some global variables before
# calling grader().  For instance, in the example f() above, the
# instructor may wish to alleviate some of the work of writing f() by
# supplying a function g() that does part of the task.  The instructor
# will then define g() before calling grader().  For convenience, any
# lines before the first % line are treated as code to be executed for
# the purpose of setting globals.

# files:

#    Answersn:  answers file, described above
#    outfile:  saved R object, containing the grades

# globals:

#    quiznum:  quiz number
#    output:  the grades so far
#    nprobs:  number of problems
#    posspts:  number of pints assigned to each problem
#    probcmds:  character vector; element i is the command to be executed
#           for problem i
#    topdir:  top directory, the one from which the code is invoked

# usage order:  
#
#    call grader() to grade the problems
#    call calcltrgrades() to compute the letter grades
#    call emailresults() to mail out the results

# uses strsplit() to separate a single string s into substrings, defined
# by fields of one OR MORE blanks, including leading and trailing
# blanks; e.g. " abc  de f" is broken into c("abc","de","f"), with no ""
# components
blanksplit <- function(s) {
   tmp <- strsplit(s," ")[[1]]
   tmp[tmp != ""]
}

# display student file on the screen; if editor is TRUE, run it through
# the Unix 'view' command
display <- function(sfname,editor=TRUE) {
   if (editor) {
      syscmd <- paste('view',sfname)
      system(syscmd)
   } else {
      charvec <- readLines(sfname,n=-1)
      for (s in charvec) cat(s,'\n')
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

###  # grade the i-th problem (counting subparts separately), using lines
###  # from the student answer file
###  gradestudentans <- function(i) {
###     # read students' source file for Problem i
###     openstudentsrcfile(i)  
###     # try to run their code
###     print("running student code")
###     runcmd <- problemlist$run[i]
###     print(paste("trying: ",runcmd))
###     try(docmd(runcmd))
###     # view student file
###     readline(paste("hit Enter when ready to go to text editor"))
###     src <- problemlist$src[i]
###     cmd <- paste("vi ",src)
###     system(cmd)
###     # closestudentfiles(i)
###     # assign score for this problem
###     fullpts <- problemlist$points[i]
###     resp <- readline(paste("pts out of ",fullpts, "? [empty means full pts] "))
###     if (resp == "") fullpts else resp
###  }

# find the subdirectories, one for each student group
getgdirs <- function() {
   gdirs <- list.dirs(recursive=F)
   gdirs[gdirs != "."]
}

###  # unpack .tar file, find student name, check for proper structure, open
###  # PDF file
###  unpack <- function(pdf) {
###     tmp <- list.files(pattern=".tar")
###     # should be just one tar file, no further subdirectories
###     if (length(tmp) != 1) {
###        print(paste("problem in subdirectory",getwd())) 
###        return(NULL)
###     }
###     # unpack .tar
###     tarname <- tmp[1]
###     cmd <- paste('system("tar xf ',tarname,'")',sep='')
###     docmd(cmd)
###     # get student name
###     sname <- strsplit(tarname,'.',fixed=TRUE)[[1]][1]
###     # open PDF
###     pdfname <- list.files(pattern=".pdf")[1]
###     pdfcmd <- paste(pdf,pdfname)
###     cmd <- paste('system("',pdfcmd,'")')
###     docmd(cmd)
###     # return student e-mail address
###     sname
###  }

# clean up the student's variables etc. so that the next student is not
# affected; just a stub for now
cleanup <- function() 0

# reads in Answersn file, sets various globals
readansfile <- function() {
   fname <- sprintf('Answers%d',quiznum)
   anslines <- readLines(fname,n=-1)
   # ftn to get first char in each line
   get1stchar <- function(line) substr(line,1,1) 
   tmp <- unlist(Map(get1stchar,anslines))
   # get the lines beginning with %
   ptslinenums <- which(tmp == '%')
   ansfilelines <- anslines[ptslinenums]
   # how many problems in this exam?
   nprobs <<- length(ansfilelines)
   # get point totals for the problems
   getpts <- function(probline) blanksplit(probline)[2]
   posspts <<- 
      as.numeric(unlist(Map(getpts,ansfilelines)))
   # get the commands for each problem, if any
   getcmd <- function(line) {
      tmp <- blanksplit(line)
      # remove problem number, pt tot, reassemble cmd
      tmp <- tmp[-(1:2)]
      Reduce(paste,tmp)
   }
   probcmds <<- unlist(Map(getcmd,ansfilelines))
   # set globals, if any
   if (ptslinenums[1] > 1) {
     write(anslines[1:(ptslinenums[1]-1)],file='tmplines') 
     source('tmplines')
   }
}

# find student answer file name
getsfname <- function(i) {
   sprintf('omsi_answer%d.txt',i)
}

backtotop <- function() {
   setwd(topdir)
}

evalr <- function(cmd) {
   eval(parse(text=cmd))
}

grader <- function(filesuff='.txt',emaildom='@ucdavis.edu',group=FALSE) {
   on.exit(exp=backtotop())
   topdir <<- getwd()  # save name of top directory
   quiznum <<- as.numeric(readline("enter quiz number: ") )
   gbls <- readline('need to Set globals? ')
   if (gbls == 'y') stop('set globals then call grader() again')
   readansfile()  # read Answersn file, setting posspts, nprobs, probcmds
   gdirs <- getgdirs()  # get student subdirectory names
   output <<- NULL
   # go through all subdirectories, one per student
   for (gdir in gdirs) {  
      cat('\n\n')
      print(paste("entering directory", gdir))
      setwd(gdir)
      cat("\n\n","  now grading",gdir,"\n")
      if (filesuff == '.txt') {
         sfname <- list.files(pattern="*.txt")
         if (length(sfname) != 1) {
            print('warning:  more than one .txt file')
            sfname <- sfname[1]
         }
         display(sfname)
      } else if (filesuff == '.tar') {
         sfname <- list.files(pattern="*.tar")
         if (length(sfname) != 1) {
            print('warning:  more than one .tar file')
            sfname <- sfname[1]
         }
         untarcmd <- paste('tar xf',sfname)
         try(system(untarcmd))
      } else stop('no .txt or .tar file')
      # get student e-mail address(es)
      emailaddr <- substr(sfname,1,nchar(sfname)-nchar(filesuff))
      if (group) {
         emailaddrs <- 
            strsplit(emailaddr,split='.',fixed=TRUE)[[1]]
      } else emailaddrs <- emailaddr
      cat("\n\n","  now grading",emailaddr,"\n")
      scores <- NULL
      outputline <- NULL
      for (prob in 1:nprobs) {
         if (!is.null(probcmds[prob])) {
            readline('hit Enter ')
            try(evalr(probcmds[prob]))
         }
         fullpts <- posspts[prob]
         tmp <- paste('score, out of ',fullpts,
            '; empty means full points: ',sep='')
         score <- readline(tmp)
            if (score == "") {
               score <- fullpts 
            } else score <- as.numeric(score)
            scores <- c(scores,score)
         outputline <- 
            paste(outputline,' ',score,'/',posspts[prob],sep='')
         }
      pen <- readline('penalty (hit Enter for 0): ')
      if (pen == "") pen <- 0 else pen <- as.numeric(pen) 
      tot <- sum(scores) - pen
      outputline <- 
         paste(outputline,' penalty = ',pen,sep='')
      outputline <- paste(outputline,' total ',tot,sep='')
      # now record the grade for this student, or all the students in
      # the group in the group = TRUE case
      for (i in 1:length(emailaddrs)) {
         tmp <- paste(emailaddrs[i],outputline)
         output <- c(output,tmp)
         print(tmp)
      }
      readline('hit Enter ')
      backtotop()
   }
   save(output,file='outfile')
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
      # emailaddr <- paste(emailaddr,"@ucdavis.edu",sep="")
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

