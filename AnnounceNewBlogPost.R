
# sends a message, "See new blog post," to the given course; 

# newpost <- function(coursename,topic,emailaddrs) 
newpost <- function(coursenum,topic,emailaddrs) 
{
   source(
    "/fandrhome/matloff/public_html/matloff/public_html/AutoGrading/MailMsg.R") 
   subject <- paste('"','new ECS',coursenum,'blog post','"')
   cat('Topic: ',topic,'\n',file='tmpmsg')
   blog <- paste0('http://heather.cs.ucdavis.edu/~matloff/',coursenum,
      '/Blog.html')
   cat(blog,'\n',file='tmpmsg',append=TRUE)
   tmp <- paste0('/fandrhome/matloff/public_html/matloff/public_html/',
      coursenum)
#    emailaddrs <- paste0(tmp,'/Beih/Grades/',
#       'Winter2018Grading',
#       '/Enrollment/EMailAddrs')
   mailmsg('tmpmsg',emailaddrs,subject)
}
