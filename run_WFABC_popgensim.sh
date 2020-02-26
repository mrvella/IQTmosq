#!/bin/bash

# This script will run data through the WFABC pipeline

# change directory
cd ~/Dropbox/GouldLab/Project_mosquito/Database

### Run the WFABC analysis
# change directory
cd ./WFABC

# set some params
data1="./WFABC_popgensim_Fit0.11-.07-.08.txt"

twoNe=1000 
min_s=-1.0
max_s=1.0
min_h=0
max_h=1

# step 1 - estimating s
~/../../Applications/WFABC_v1.1/binaries/Mac/wfabc_1 -nboots 0 $data1

  
# step 2 - estimating h
~/../../Applications/WFABC_v1.1/binaries/Mac/wfabc_2 \
  -fixed_N $twoNe -min_s $min_s -max_s $max_s \
  -min_h $min_h -max_h $max_h \
  $data1




