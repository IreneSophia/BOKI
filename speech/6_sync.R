# (C) Irene Sophia Plank (10planki@gmail.com)

# This script calculates synchrony in two ways. First, it uses the CCF function 
# of the rMEA package to calculate windowed cross-lagged correlations of the 
# continuous pitch and intensity values of two speakers interacting with each 
# other. Second, it takes the information produced by the 5_turns.R script and
# calculates the turn-based adaptation in pitch, intensity and articulation rate 
# of one speaker to the other. To do so, the median pitch and intensity per turn
# are calculated. 

# Set WD and load libraries -----------------------------------------------

setwd("/home/emba/Documents/ML_BOKI/Data_speech/")

library(tidyverse)
library(rMEA)

source("/home/emba/Documents/ML_speech/MLSPE_scripts/fakeMEA_function.R")

# Load data and set up output matrices ------------------------------------

cols = c("name", "pit_sync_MEA", "int_sync_MEA")
df.dyad = data.frame(matrix(nrow = 0, ncol = length(cols)))
colnames(df.dyad) = cols

df.turn = read_csv("ML_turns.csv")
df.turn$name = paste(df.turn$dyad, df.turn$task, sep = "_")

# get rid of turns that start in the first 10 seconds
df.turn = df.turn %>% filter(start_turn > 10000)

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
    fl_L = list.files(pattern = paste("^ch_L.*", df.turn$dyad[i] ,".*",
                                      df.turn$task[i], ".*cont\\.csv$",
                                      sep = ""))
    fl_R = list.files(pattern = paste("^ch_R.*", df.turn$dyad[i] ,".*",
                                      df.turn$task[i], ".*cont\\.csv$",
                                      sep = ""))
    df_L = read_delim(fl_L, delim = ";") %>%
      mutate(across(time, ~ .x * 1000)) %>%
      mutate(across(time, round)) %>%
      mutate(across(where(is.character), as.numeric))
    df_R = read_delim(fl_R, delim = ";") %>%
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

write_csv(df.turn, "ML_turns.csv")
write_csv(df.indi, "ML_sync-indi.csv")
write_csv(df.dyad, "ML_sync-dyad.csv")
