
# code to extract individual problem scores from quiz records

getscores <- function(infile) {
   scoredata <- read.table(infile,stringsAsFactors=FALSE)
   ncolstot <- ncol(scoredata)
   indivscorecols <- 2:(ncolstot-7)
   indivscores <- NULL
   for (colnum in indivscorecols) {
      indivscores <- 
         cbind(indivscores,getnumerators(scoredata[,colnum]))
   }
   totscores <- as.numeric(scoredata[,ncolstot-1])
   scores <- cbind(indivscores,totscores)
   row.names(scores) <- NULL
   scores
}

getnumerators <- function(scorecol) {
   scsplit <- function(score) {
      tmp <- strsplit(score,split='/')
      as.numeric(tmp[[1]][1])
   }
   unlist(Map(scsplit,scorecol))
}


