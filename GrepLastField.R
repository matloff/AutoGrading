
# intended app:

# have grades file, indexed by student e-mail address, one line per
# student, blank-separated, with grade (exam, homework, whatever) in the
# last field, presumed character mode; extract it

# arguments:

#    fname: input file
#    idx: index, e.g. student e-mail address

grepgetlast <- function(fname,idx) {
   f <- scan(file=fname,what='',sep='\n')
   i <- grep(idx,f)
   rec <- f[i]
   tmp <- strsplit(rec,split=' ')[[1]]
   tmp[length(tmp)]
}

