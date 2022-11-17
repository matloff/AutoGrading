
echo "will produce roster in 'tmp', e-mail list in 'email'"
echo "make sure TA address includes '@ucdavis.edu"

echo "go to https://lobster.ucdavis.edu/rosters/html/ros_subj.cfm"
echo "copy and paste lines: then ctrl-d"
cat - >! tmp

python /fandrhome/matloff/public_html/matloff/public_html/AutoGrading/GetEMailBest.py tmp yes >! email

# add TA address, entering 
echo "enter TA address, then ctrl-d"
cat - >> email




