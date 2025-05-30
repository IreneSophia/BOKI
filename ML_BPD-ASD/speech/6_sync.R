# (C) Irene Sophia Plank (10planki@gmail.com)

# This script calculates synchrony in two ways. First, it uses the CCF function 
# of the rMEA package to calculate windowed cross-lagged correlations of the 
# continuous pitch and intensity values of two speakers interacting with each 
# other. Second, it takes the information produced by the 5_turns.R script and
# calculates the turn-based adaptation in pitch, intensity and articulation rate 
# of one speaker to the other. To do so, the median pitch and intensity per turn
# are calculated. 

library(tidyverse)
library(rMEA)

# Initialize function to create a fake MEA object out of two vectors
# Input: 
#     * s1, s2: numeric vectors containing the values to be correlated
#     * sampRate: sampling rate per second
#     * s1Name, s2Name: name for the values to be correlated, default is "s1Name" and "s2Name"
# Output:
#     * fake MEA object that pretends to be a MEA object
#
fakeMEA = function(s1, s2, sampRate, s1Name = "s1Name", s2Name = "s2Name") {
  mea = structure(list(all_01_01 = structure(list(MEA = structure(list(
    s1Name = s1, s2Name = s2), row.names = c(NA, -length(s1)), class = "data.frame"), 
    ccf = NULL, ccfRes = NULL), id = "01", session = "01", group = "all", sampRate = sampRate, 
    filter = "raw", ccf = "", s1Name = s1Name, s2Name = s2Name, uid = "all_01_01", 
    class = c("MEA","list"))), class = "MEAlist", nId = 1L, n = 1L, groups = "all", sampRate = sampRate, 
    filter = "raw", s1Name = s1Name, s2Name = s2Name, ccf = "")
  return(mea)
}

dt.path = c("/media/emba/emba-2/ML_BOKI/AUD_preprocessed", 
            "/media/emba/emba-2/ML_BOKI/ML_data")

# Load data and set up output matrices ------------------------------------

cols = c("name", "pit_sync_MEA", "int_sync_MEA")
df.dyad = data.frame(matrix(nrow = 0, ncol = length(cols)))
colnames(df.dyad) = cols

df.turn = read_csv(file.path(dt.path[1], "OUT_turns.csv"))
df.turn$name = paste(df.turn$dyad, df.turn$task, sep = "_")

# no need to get rid of the first 10 seconds > was already cut in praat
# but for index reasons the 0 as start time is replaced by 1
df.turn = df.turn %>%
  mutate(
    start_turn = if_else(start_turn==0,1,start_turn)
  )

# Calculate average pitch and intensity -----------------------------------

df.turn$pit_turn = NA
df.turn$int_turn = NA

# go through all the turns
for (i in 1:nrow(df.turn)) {
  # first, load the respective data if...
  if (i != 1) {
    if (df.turn$name[i] != df.turn$name[i-1]) { # it is a new name or...
      reload = T
    }
  } else { # it is the first name
    reload = T
  }
  if (reload) {
    fl_L = list.files(pattern = paste(df.turn$dyad[i] ,"_",
                                      df.turn$task[i], "_ch_L",
                                      ".*cont\\.csv$",
                                      sep = ""),
                      path = dt.path[1])
    fl_R = list.files(pattern = paste(df.turn$dyad[i] ,"_",
                                      df.turn$task[i], "_ch_R",
                                      ".*cont\\.csv$",
                                      sep = ""),
                      path = dt.path[1])
    df_L = read_delim(file.path(dt.path[1], fl_L), delim = ",") %>%
      mutate(across(time, ~ .x * 1000)) %>%
      mutate(across(time, round)) %>%
      mutate(across(where(is.character), as.numeric))
    df_R = read_delim(file.path(dt.path[1], fl_R), delim = ",") %>%
      mutate(across(time, ~ .x * 1000)) %>%
      mutate(across(time, round)) %>%
      mutate(across(where(is.character), as.numeric))
    lst = list(L = df_L, R = df_R)
    reload = F
    print(df.turn$name[i])

    # Compute MEA synchrony ---------------------------------------------------

    # resample data and create fakeMEA objects
    sampRate = 100
    s1 = df_L$pitch
    s1[is.na(s1)] = 0
    s1 = signal::resample(s1,1,1000/sampRate) # resample to 100Hz
    s2 = df_R$pitch
    s2[is.na(s2)] = 0
    s2 = signal::resample(s2,1,1000/sampRate) # resample to 100Hz
    mea_pit  = fakeMEA(s1,s2,sampRate,"pit_L","pit_R")
    s1 = df_L$int
    s1[is.na(s1)] = 0
    s1 = signal::resample(s1,1,1000/sampRate) # resample to 100Hz
    s2 = df_R$int
    s2[is.na(s2)] = 0
    s2 = signal::resample(s2,1,1000/sampRate) # resample to 100Hz
    mea_int  = fakeMEA(s1,s2,sampRate,"int_L","int_R")

    # compute synchrony - win and inc based on Ochi (2019)
    mea_pit = MEAccf(mea_pit,2,16,8) # lagSec, winSec, incSec
    mea_int = MEAccf(mea_int,2,16,8)

    # creating new rows with mean of peaks of windows
    new = c(df.turn$name[i],
            mean(apply(mea_pit$all_01_01$ccf,1,max)),
            mean(apply(mea_int$all_01_01$ccf,1,max)))

    df.dyad[nrow(df.dyad) + 1,] = new
  }

# Compute turn-based pitch and intensity averages -------------------------
  
  # get average pitch and intensity from this turn
  idx_turn_start = which(lst[[df.turn$speaker[i]]]$time == df.turn$start_turn[i])
  idx_turn_end   = which(lst[[df.turn$speaker[i]]]$time == df.turn$end_turn[i])
  
  df.turn$pit_turn[i] = median(lst[[df.turn$speaker[i]]]$pitch[idx_turn_start:idx_turn_end], na.rm = T)
  df.turn$int_turn[i] = median(lst[[df.turn$speaker[i]]]$int[idx_turn_start:idx_turn_end], na.rm = T)
  
}

# Compute turn-based synchrony and synchronisation ------------------------

# add information on previous turn or last window of previous turn
df.turn$pit_prev_turn = NA
df.turn$int_prev_turn = NA
df.turn$art_prev_turn = NA
for (i in 2:nrow(df.turn)) {
  if (df.turn$name[i] == df.turn$name[i-1]) {
    df.turn$pit_prev_turn[i] = df.turn$pit_turn[i-1]
    df.turn$int_prev_turn[i] = df.turn$int_turn[i-1]
    df.turn$art_prev_turn[i] = df.turn$art_turn[i-1]
  }
}

# order data by name, speaker and then start of the turn
df.turn = df.turn[with(df.turn, order(name, speaker, start_turn)),]

# summarise the data with absolute r between previous and turn averages to 
# compute individual synchronisation for each task separately
df.indi = df.turn %>% group_by(name, speaker) %>% 
  summarise(
    pit_sync   = abs(cor(pit_prev_turn, pit_turn, use = 'na.or.complete')),
    int_sync   = abs(cor(int_prev_turn, int_turn, use = 'na.or.complete')),
    art_sync   = abs(cor(art_prev_turn, art_turn, use = 'na.or.complete'))
  )

# Save synchronisation data frame -----------------------------------------

write_csv(df.turn, file.path(dt.path[1], "OUT_turns.csv"))
write_csv(df.indi, file.path(dt.path[1], "OUT_sync-indi.csv"))
write_csv(df.dyad, file.path(dt.path[1], "OUT_sync-dyad.csv"))
