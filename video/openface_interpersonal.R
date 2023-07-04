# (C) Irene Sophia Plank
# 
# This script takes the output of OpenFace and calculates interpersonal 
# synchrony. It is an adaptation of a script written by Jana Koehler, 
# published in https://github.com/jckoe/MLASS-study. 

# clean workspace
rm(list=ls())

# load libraries
library(tidyverse)
library(rMEA)
library(data.table)    # setDT
library(moments)       # kurtosis, skewness

# set path to MEA files
dt.path = "/home/emba/Documents/ML_BOKI/Data_OpenFace"

options(datatable.fread.datatable = F)

# Read in data ------------------------------------------------------------

# lists all relevant files
ls.fls    = list.files(path = dt.path, recursive = T, pattern = "BOKI_.*\\.csv", full.names = T, include.dirs = F) 

# reads in the data
ls        = lapply(ls.fls, fread, 
                   header = F, # preserves columns for frame number
                   skip = 301, # skips first 10 seconds (b/c of MEA artefacts)
                   # timestamp, confidence, success, position (pitch, yaw, roll), 
                   # intensity and occurrence of different AUs
                   drop = c(2,6:296,300:679)) 

# drop path and file extension from file list
ls.fls = substr(ls.fls, nchar(dt.path)+2, nchar(dt.path)+12)

names(ls) = ls.fls

# Preprocessing -----------------------------------------------------------


# Time series synchronisation ---------------------------------------------



# Movement quantity -------------------------------------------------------

