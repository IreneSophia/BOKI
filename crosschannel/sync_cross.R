# (C) Irene Sophia Plank
# 
# This function computes intra- or interpersonal synchrony between time courses
# of two different channels (crosschannel synchrony). It uses the rMEA CCF 
# function for the synchrony computation and the resample function of the signal
# package. 
# Input: 
#     * s1, s2: time series
#     * sr1, sr2: sampling rates of the two time series
#     * sampRate: sampling rate to which both time series should be resampled
#     * lagSec, winSec, incSec: length of lag, window and increment in seconds
#     * r2Z: whether to apply z transformation (BOOLEAN, default T)
#     * ABS: whether to report absolute values (BOOLEAN, default T)
# Output:
#     * mea: MEA object containing resampled time series and CCF sync values
#

# Packages ----------------------------------------------------------------

# if packman is not installed yet, install it
if(!("pacman" %in% installed.packages()[,"Package"])) install.packages("pacman")
pacman::p_load(tidyverse, rMEA, signal) 

# Function ----------------------------------------------------------------

sync_cross = function(s1, s2, sr1, sr2, sampRate, lagSec, winSec, incSec, 
                          r2Z = T, ABS = T) {
  
  # resample the time series
  s1rs = resample(s1, sampRate, sr1)
  s2rs = resample(s2, sampRate, sr2)
  
  # create fakeMEA object
  mea = structure(list(all_01_01 = structure(list(MEA = structure(list(
    s1Name = s1rs, s2Name = s2rs), row.names = c(NA, -length(s1)), class = "data.frame"), 
    ccf = NULL, ccfRes = NULL), id = "01", session = "01", group = "all", sampRate = sampRate, 
    filter = "raw", ccf = "", s1Name = s1Name, s2Name = s2Name, uid = "all_01_01", 
    class = c("MEA","list"))), class = "MEAlist", nId = 1L, n = 1L, groups = "all", sampRate = sampRate, 
    filter = "raw", s1Name = s1Name, s2Name = s2Name, ccf = "")
  
  mea = MEAccf(mea, lagSec, winSec, incSec, r2Z = r2Z, ABS = ABS)
  
  return(mea)
  
}