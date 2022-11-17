
import os

def nonl(s):
   return s.rstrip('\n')

# extracts the student e-mail address, minus @ucdavis.edu, from a line
# in the grades file
def getaddr(gradesline):
   return gradesline.split[0]

def main():
   gradesfile= raw_input('enter grades file name: ')
   recs = open(gradesfile)
   recs = recs.readlines()
   for rec in recs:
      toaddr = rec.split()[0] + '@ucdavis.edu'
      recfile = open('tmprec','w')
      recfile.write(rec+'\n')
      recfile.close()
      subject = 'quiz results'
      mailcmd = 'mail -s "' + subject + '"'
      mailcmd += ' ' + toaddr + ' < tmprec'
      print 'sending: ' + mailcmd
      os.system(mailcmd)
      os.unlink(os.path.abspath('tmprec'))
      # might want to increase this; check "throttle e-mail" in Google
      os.system('sleep 5')  

if __name__ == '__main__': main()

