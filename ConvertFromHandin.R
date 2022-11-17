
# Call the directory from which this script is invoked the top
# directory.  It is assumed to have subdirectories, one for each student
# group.  For each one, the script (a) descends into the subdirectory;
# (b) attempts to untar the .tar file; (c) find the basename of the .tar
# file (official UCD e-mail addresses, concatenated via periods); (d)
# ascends to the top directory; (e) changes the subdirectory name to the
# basename.

convert <- function() {
   topdir <<- getwd()
   on.exit(setwd(topdir))
   for (d in setdiff(list.dirs(),'.')) {
      setwd(d)
      cat('working in',d,'\n')
      ntar <- 0
      for (fl in list.files()) {
         bn <- getbase(fl,'.tar')
         if (!is.na(bn)) {
            tarbase <- bn
            ntar <- ntar + 1
         }
      }
      if (ntar != 1) {
         print('invalid .tar setup')
      } else system('tar xf *.tar')
      setwd(topdir)
      if (ntar == 1) file.rename(d,tarbase)
   }
}

# for given suffix, e.g. '.tar', returns the prefix, i.e. file name
# fname without the suffix; NA return if suffix not there
getbase <- function(fname,suff) {
   lfn <- nchar(fname)
   startsuff <- lfn - nchar(suff) + 1
   if (substr(fname,startsuff,lfn) != suff) return(NA)
   return(substr(fname,1,startsuff-1))
}

