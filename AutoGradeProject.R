
# assumes a Unix-family machine, i.e. Mac or Linux

# script to semi-automate project grading; see HOW TO RUN below for an
# overview 

# IMPORTANT DIRECTIONS:

#  1.  It is assumed that in the current directory: 
#
#     * there is a subdirectory for each student group  
#     * within each subdirectory, there is a .tar file 
#     * the .tar file contains the group's files (.tex, .jpg, .R, etc.), 
#       that will be unpacked in the SAME directory, i.e. no further 
#       subdirectories 
#     * the report PDF must be named ProjectReport.pdf
#
#  2.  For each group, the script will:
#
#     * descend into the group's directory
#     * unpack the .tar file
#     * run the Unix ls via R's system()
#     * ask the grader to read the desired PDF file 
#     * ask the grader for comments on the group's report
#     * ask the grader for the grade on the group's report
#     * print the results to the screen and to outfile, with the
#       following lines for each student in the group::
#        - a line showing the e-mail address and project grade
#        - a line showing the comments (NO e-mail address)
#     * ascend back to the original directory

# find the subdirectories, one for each student group
getgdirs <- function() {
   gdirs <- list.dirs(recursive=F)
   gdirs[gdirs != "."]
}

# unpack .tar file, find student names, check for proper structure
unpack <- function() {
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
      print(paste("problem in subdirectory",getwd())) 
      return(NULL)
   }
   # unpack the group's tar file
   system("tar xf *.tar")
   grpname <- substr(tmp,1,ntmp-4)
   emailaddrs <- strsplit(grpname,".",fixed=T)
   list(grpname=grpname, emailaddrs=emailaddrs)
}

# pdfcmd is the PDF view you wish to use
grader <- function(pdfcmd='open') 
{
   print('NOTE: best run under "script" in case something goes wrong')
   askOK <- paste0('the PDF viewer command is ',pdfcmd,', OK? ')
   ok <- readline(askOK)
   if (substr(ok,1,1) != 'y') stop('exiting due to no PDF command')
   basedir <- getwd()
   on.exit(setwd(basedir))
   gdirs <- getgdirs()  # get group subdirectory names
   print('checking for continuation of previous grading session')
   if (file.exists('outfile')) {
      load('outfile')
      readline('outfile exists; hit Enter when ready to view')
      edit(outputWithNotes)
      resp <- readline('initialize "outputWithNotes" to this? ')
      if (substr(resp,1,1) == 'y') {
         outputWithNotes <<- outputWithNotes
         outputGradesOnly <<- outputGradesOnly
         rm(outputWithNotes,outputGradesOnly)
      } 
   } 
   if (!exists('outputWithNotes',.GlobalEnv)) {
      outputWithNotes <<- NULL
      outputGradesOnly <<- NULL
      print('"output*" initialized to NULL')
   }
   # go through all subdirectories. one per group
   for (gdir in gdirs) {  
      print(paste("\n entering directory", gdir))
      setwd(gdir)
      # try to open this group's .tar file, check for subdirectories
      # (not allowed`), get student names etc.
      tmp <- unpack()
      if (is.null(tmp)) {
         print(paste("unpack() had problems in: ", gdir))
         print("giving up; NOTE PENALTY, grade separately later")
         grade <- 'I'
         emailaddrs <- gdir
         cmntsline <- 'COULD NOT UNPACK'
      } else {
         # original tar file name, without ".tar"
         grpname <- tmp$grpname
         # vector of individual e-mail addresses, without @ucdavis.edu
         emailaddrs <- tmp$emailaddrs
         cat("\n\n","  now grading",grpname,"\n")
         # see what files are there; do they have the proper files,
         # including .tex?; which PDF to view?
         if (!checkForFiles()) {
            print('************** no .pdf file')
         } else {
            pdfFile <- 'ProjectReport.pdf'
            cmd <- paste(pdfcmd,pdfFile)
            print(cmd)
            readline("hit Enter to read PDF file, EXIT reader when done ")
            system(cmd)
         }
         print('enter comments, then ctrl-d')
         cmnts <- readLines()
         cmnts <- paste(cmnts,collapse=';')
         cmntsline <- paste("# ",cmnts)
         grade <- readline("enter grade:  ") 
         if (!grade %in% 
           c('A+','A','A-','B+','B','B-','C+','C','C-','D+','D','D-','F'))
           stop('invalid grade')
      }
      # definitely will reach here unless could not unpack
      for (emaddr in emailaddrs[[1]]) {
         studentgradeline <- paste(emaddr,grade)
         print(studentgradeline)
         print(cmntsline)
         outputWithNotes <<- c(outputWithNotes,studentgradeline,cmntsline)
         outputGradesOnly <<- c(outputGradesOnly,studentgradeline)
      }

      # leave dir for this group, back to top level
      setwd(basedir)

      # backup results
      save(outputWithNotes,outputGradesOnly,file='outfile')
      plain_gdir <- substr(gdir,3,nchar(gdir))
      ofname <- paste0('outfile.',plain_gdir)
      system(paste0('cp outfile ../',ofname))

      cat('output length = ',length(outputGradesOnly),'\n')
      readline('Enter or ctrl-c')

   }
   print(outputWithNotes)
   repeat {
      if (readline("need to edit? ") == "y") {
         outputWithNotes <<- edit(outputWithNotes)
         outputGradesOnly <<- edit(outputGradesOnly)
      } else break
   }
   cat("\n","results:","\n")
   save(outputWithNotes,outputGradesOnly,file="outfile")
   cat(outputWithNotes,file='ProjectGrades',sep='\n')
}

checkForFiles <- function() {
         fls <- list.files()
         suffixes <- getSuffixex(fls)
         if (!('R' %in% suffixes)) 
            print('no .R file')
         if (!('tex' %in% suffixes)) 
            print('no .tex file')
         if (!('jpg' %in% suffixes) && !('JPG' %in% suffixes) &&
             !('png' %in% suffixes) && !('PNG' %in% suffixes))
            print('no image files')
         file.exists('ProjectReport.pdf')
}

emailresults <- function() {
   # if sending mail requires a password, remind the user
   readline("set password externally (if any), then hit Enter")
   load("outfile")
   for (l in output) {
      tmp <- strsplit(l," ")[[1]]
      if (tmp != "#") {
         emailaddr <- tmp[1]
         emailaddr <- paste(emailaddr,"@ucdavis.edu",sep="")
         cat(l,file="onestudent")
         tosend <- paste("mutt",emailaddr,"-s 'project results' < onestudent")
         system(tosend)
         print(l)
         system("sleep 10")
         system("/bin/rm onestudent")
      }
   }
}

# evaluate the given R expression; note: set any needed globals,
# including functions, at the R prompt before calling grader(); also
# note: in principle, a student can put multiple R statements within one
# line, separated by semicolons, but it is not recommended to have them
# write full code, e.g. full functions--it almost always has syntax
# errors etc.
evalrstring <- function(toexec) {
   cat(toexec,file="tmpexec")
   sourceresult <- try(source("tmpexec"))
   if (class(sourceresult) == "try-error" || 
      !is.numeric(sourceresult$value)) {
      print("R parse error at:")
      print(toexec)
      return(NA)
   } else return(sourceresult$value)
}

# extract suffixes from file names
getSuffixex <- function(fnames) 
{
    tmp <- strsplit(fnames,'.',fixed=TRUE)
    getLast <- function(vec)  # extract last elt in vec 
       vec[length(vec)]
    sapply(tmp,getLast)
}
