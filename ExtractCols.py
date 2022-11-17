
# inputs file with blank-separated field, same number of fields per
# record, except for comment lines ('# ....')

# outputs selected fields, skipping comment lines

# usage:  

#   python ExtractFields.py input_file col_nums_from_0

# negative col nums mean from right end; right end is -1, to its left is
# -2 etc.

import sys

f = open(sys.argv[1])
colnums = map(eval,sys.argv[2:])
ncols = len(colnums)
def xformneg(idx):
   if (idx >= 0): return idx
   else: return ncols + idx

for l in f.readlines():
   words = l.split()
   if words[0] != '#':
      for i in colnums: print words[i],
      print

