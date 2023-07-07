# (C) Irene Sophia Plank
# 
# This function uses an approach described in Moulder et al. 
# (2018, Psychol Methods) to test the following hypothesis: "Synchrony does not 
# exist between sections of size m in these two time series" (section sliding). 
# This segment shuffling, from Moulder et al., "requires researchers to cut a 
# time series X into shorter sections of size m which are randomly appended to 
# one another to create a new time series Xs until no section is in its original 
# position". This shuffled time series is then paired with an unchanged time 
# series Y. Pseudosynchrony is calculated using the CCF function from rMEA. 
# Input: 
#     * mea: a list of MEA objects, can also be created using the fakeMEA function
#     * sampRate: sampling rate per second
#     * n: how many permutations to perform per MEA object
#     * lagSec, winSec, incSec: length of lag, window and increment in seconds
#     * r2Z: whether to apply z transformation (BOOLEAN, default T)
#     * ABS: whether to report absolute values (BOOLEAN, default T)
# Output:
#     * df.psync: data frame in long format containing pseudosync values
#

pseudosync_seg = function(mea, sampRate, n, lagSec, winSec, incSec, r2Z = T, ABS = T) {
  
  return(df.psync)
}