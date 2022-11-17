
# inputs Registrar class list, outputs e-mail addresses

# usage:  

#   python GetEMailBest.py input_file 

#      where if the last is nonempty, then @ucdavis.edu will be added

# input file assumed to be .csv

# outputs e-mail addresses to stdout 

import sys

f = open(sys.argv[1])
splitChar = ','
if len(sys.argv) > 2: splitChar = sys.argv[2]
for l in f.readlines():
   words = l.split(splitChar)
   if 'RE' in words or 'WL' in words:
         email = words[-1]
         print email,  # comma avoids extra EOL

