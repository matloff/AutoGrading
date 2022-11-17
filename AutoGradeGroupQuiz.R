
# script to semi-automate group quiz (full code submitted) grading of
# programming problems; of course, for nongroups, group size is 1, and
# everything works

# NOTE:

# In this version, the grader is asked to input commands, which may
# differ from one group to another, e.g. source('3.R'); print(f3); this
# means more cutting and pasting on the grader's part, but fewer
# problems due to students not following directions.  So, for a given
# problem, the grader will be repeatedly asked to give a command, just
# hitting Enter when done giving commands for that problem.  The grader
# is then prompted to enter the score for that problem.

# IMPORTANT DIRECTIONS:

# This script will be invoked in a directory that has one subdirectory
# for each group, which is the form created by the CSIF handin program.
# The script will descend into each one, grading the group there.

# The student group's file must be named in the format groupnames.tar,
# where groupnames consists of the students' UCD e-mail addresses (minus
# the "@ucdavis.edu"), separated by periods.  There must be no
# subdirectories archived within the file.

# Penalties may be imposed for failure to follow directions.  

# usage order:  
#
#    call grader() to grade the problems
#    call calcltrgrades() to compute the letter grades
#    call emailresults() to mail out the results
#
#    one of the output files will be named QuizGroup, containing the
#    full results
#
#    important note: after each of the calls to grader() and
#    calcltrgrades(), the R variable output is saved to the file outfile
#    (in R object format); the latter can be copied to another machine
#    before calling emailresults()

# globals:
#
#    pttots: vector of max points for each problem
#    output:  quiz results minus letter grades
#    basedir:  directory from which script is originally invoked; "trail
#              of bread crumbs" in case crash
#
# note that in case of a crash of some kind, the global variable output
# serves as a checkpoint, so that one need not start again from scratch

# uses strsplit() to separate a single string s into substrings, defined
# by fields of one OR MORE blanks, including leading and trailing
# blanks; e.g. " abc  de f" is broken into c("abc","de","f"), with no ""
# components
blanksplit <- function(s) {
   tmp <- strsplit(s," ")[[1]]
   tmp[tmp != ""]
}

# easier just to have grader write gradestudentans()!; here we just
# prompt

gradestudentans <- function(i) {
   print(paste("grading problem ", i))
   repeat {
      cmd <- readline("type unquoted command, e.g. ls() or Enter to finish:  ")
      if (cmd == '') break
      try(docmd(cmd))
   }
   fullpts <- pttots[i]
   resp <- readline(paste("pts out of ",fullpts, "? [empty means full pts] "))
   if (resp == "") pts <- fullpts else pts <- resp
   pts
}

# find the subdirectories, one for each student group
getgdirs <- function() {
   gdirs <- list.dirs(recursive=F)
   gdirs[gdirs != "."]
}

# unpack .tar file, find student names, check for proper structure
unpack <- function() {
   print('files present:')
   print(list.files())
   tmp <- list.files(pattern=".tar")
   # should be just one tar file, no further subdirectories
   if (length(tmp) != 1) {
      print(paste("problem in subdirectory",getwd())) 
      return(NULL)
   }
   grpname <- tmp[1]
   tmp <- grpname
   ntmp <- nchar(tmp)
   # not a .tar file?
   if (substr(tmp,ntmp-3,ntmp) != ".tar") {
      print(paste("no tar file in subdirectory",getwd())) 
      return(NULL)
   }
   # unpack the students' tar file
   system("tar xf *.tar")
   grpname <- substr(tmp,1,ntmp-4)
   emailaddrs <- strsplit(grpname,".",fixed=T)
   list(grpname=grpname, emailaddrs=emailaddrs)
}

# clean up the student's variables etc. so that the next student is not
# affected
cleanup <- function(i) {
   # system('/bin/rm -f *')
}

grader <- function() {
   tmp <- readline("enter point totals for each problem:  ") 
   pttots <<- strsplit(tmp," ")[[1]]
   basedir <<- getwd()  # trail of bread crumbs
   print(paste("currently in directory", basedir))
   gdirs <- getgdirs()  # get group subdirectory names
   output <<- NULL  # ultimately will be grade records
   # go through all subdirectories. one per group
   for (gdir in gdirs) {  
      print("\n\n")
      print(paste("entering directory", gdir))
      setwd(gdir)
      # try to open this group's .tar file, get student names etc.
      tmp <- unpack()
      if (is.null(tmp)) {
         print(paste("unpack() had problems in: ", gdir))
         print("giving up; note penalty, grade separately later")
         setwd(basedir)
         next;
      }
      # original tar file name, without ".tar"
      grpname <- gdir
      # vector of individual e-mail addresses, without @ucdavis.edu
      emailaddrs <- tmp$emailaddrs
      cat("\n\n","  now grading",grpname,"\n")
      total <- 0
      outputline <- NULL
      for (i in 1:length(pttots)) {
         score <- gradestudentans(i)
         outputline <- 
            paste(outputline," ", score,"/",pttots[i],sep="")
         total <- total + as.integer(score)
         cleanup(i)
      }
      penalty <- 0
      penalty <- readline("penalty, e.g. late [0]:  ") 
      penalty <- if (penalty == "") 0 else as.integer(penalty)
      total <- total - penalty
      outputline <- paste(outputline,"penalty =",penalty)
      outputline <- paste(outputline,"total =",total)
      for (emaddr in emailaddrs[[1]]) {
         studentline <- paste(emaddr,outputline)
         output <<- c(output,studentline)
      }
      print(output)
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
   load("outfile")
   tmp <- readline("enter cutoffs (none for F), e.g. 95 A+ 85 A 70 B...: ")
   tmp <- strsplit(tmp," ")[[1]]
   inds <- 1:length(tmp)
   evens <- inds[inds %% 2 == 0]
   odds <- inds[inds %% 2 == 1]
   ltrgrades <- tmp[evens]
   cutoffs <- as.integer(tmp[odds])
   # totcol <- length(strsplit(output[1]," ")[[1]]) 
   totcol <- length(blanksplit(output[1])) 
   for (i in 1:length(output)) {
      # total <- strsplit(output[i]," ")[[1]][totcol]
      total <- blanksplit(output[i])[totcol]
      total <- as.integer(total)
      ltrgrd <- num2ltr(total,cutoffs,ltrgrades)
      output[i] <- paste(output[i],ltrgrd)
   }
   # save to file for records
   save(output,file="outfile")
   write(output,file="GroupQuizGrades")
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

