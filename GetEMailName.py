
# inputs Registrar class list, outputs e-mail addresses

# usage:  

#   python GetEMailBest.py input_file add_ucd

#      where if the last is nonempty, then @ucdavis.edu will be added

# outputs e-mail addresses to stdout 

import sys

f = open(sys.argv[1])
for l in f.readlines():
   words = l.split()
   email = words[-1]
   print email, words[2], words[3]

