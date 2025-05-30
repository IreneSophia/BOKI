# (C) Irene Sophia Plank
# 
# This script takes lists of MEA and fakeMEA objects and calculates pseudo-
# synchrony using the pseudosync_seg function. 

library(tidyverse)
library(rMEA)

# clean workspace
rm(list = ls())

# load pseudosync_seg function
source("pseudosync_seg.R")

# set working directory
setwd("/home/emba/Documents/ML_BOKI/Data_psync")

# list of file names which contain the lists: file names have sampRate, lag, win, inc
ls.files = dir(pattern = "*.RDS")

# create a dataframe
df.psync = data.frame()

# set number of permutations
n = 1

# loop through the file names
for (f in ls.files) {
  print(sprintf("Processing %s", gsub(".RDS", "", f)))
  mea = readRDS(f)
  set = str_split(gsub(".RDS", "", f), pattern = "_")
  df.mea = pseudosync_seg(mea, 
                          sampRate = as.numeric(set[[1]][2]), 
                          lagSec   = as.numeric(set[[1]][3]),
                          winSec   = as.numeric(set[[1]][4]),
                          incSec   = as.numeric(set[[1]][5]),
                          n = n)
  df.mea$mod = set[[1]][1]
  df.psync = rbind(df.psync, df.mea)
}

# save the pseudosynchrony values
write_csv(df.psync, "df_psync.csv")
