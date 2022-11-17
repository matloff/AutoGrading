
# inputs class roster from Registrar, outputs e-mail addresses

# usage:  

#   python GetEMail.py input_file 

# outputs e-mail addresses to stdout 

import sys

f = open(sys.argv[1])
for l in f.readlines():
   w = l.split()
   print w[len(w)-1]+'@ucdavis.edu'

