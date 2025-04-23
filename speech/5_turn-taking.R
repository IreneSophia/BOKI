# (C) Irene Sophia Plank
# 
# This script takes the output of the uhm-o-meter (de Jonge et al.) and 
# identifies turns based on the silences and sounding instances. Specifically, 
# all sounding instances that are completely engulfed by the counterpart's 
# sounding instance are disregarded. Then, a turn goes from the start of the 
# first until the end of the last consecutive sounding instance of one speaker. 
# Based on these turns, the script calculates turn-taking gaps between turns in 
# milliseconds. Additionally, the script counts the syllables detected by the 
# uhm-o-meter per turn and based on this calculates the articulation rate of the
# respective turn. 

library(tidyverse)

dt.path = c("/media/emba/emba-2/ML_BOKI/AUD_preprocessed", 
            "/media/emba/emba-2/ML_BOKI/ML_data")


# Get list of included dyads ----------------------------------------------

# lists all relevant IDs
df.sub = read_csv(file.path("/media/emba/emba-2/ML_BOKI/demoCentraXX", 
                            "BOKI_centraXX.csv")) %>%
  filter(substr(dyad, 1, 4) == "BOKI") %>%
  select(dyad, ID, label)
ls.inc = unique(df.sub$dyad)
ls.IDs = c(paste0(ls.inc, "_M_ch_L"), paste0(ls.inc, "_H_ch_L"),
           paste0(ls.inc, "_M_ch_R"), paste0(ls.inc, "_H_ch_R"))

# Import silence data -----------------------------------------------------

# check if any silence files are missing
incomplete = c()
ls.fls = c() # list for all relevant files
for (d in ls.IDs) {
  file = file.path(dt.path[1], paste0(d, '_silence.csv'))
  if (!file.exists(file)) {
    warning(sprintf('File %s does not exist', d))
    incomplete = c(incomplete, d)
  } else {
    ls.fls = c(ls.fls, file)
  }
}

# no incomplete files

# load all the data into one data frame
df  = do.call("rbind", lapply(ls.fls, read_csv, show_col_types = F))

# convert to ms
df = df %>% mutate(across(where(is.numeric), ~ .x * 1000)) %>% 
  mutate(across(where(is.numeric), round)) %>%
  separate(col = Name, into = c("dyad1", "dyad2", "task", "ch", "side"), remove = T) %>%
  mutate(
    dyad = paste0(dyad1, "_", dyad2)
  ) %>%
  select(dyad, side, task, Label, Start, End, Duration) %>%
  rename(
    "code" = "Label", "xmin" = "Start", "xmax" = "End", "dur" = "Duration"
  )

# Turn-taking gap ---------------------------------------------------------

# order data by dyad, task and then sounding instance
df = df[with(df, order(dyad, task, xmin, xmax)),]

# only look at the sounding instances
df.sound = df %>% filter(code == "sounding")

# we need to get rid of all sounding instance that are completely engulfed in another
df.sound$delete = F
for (i in 2:nrow(df.sound)) {
  if (sum((df.sound$xmin[i] >= df.sound[(df.sound$dyad == df.sound$dyad[i]) &
                                    (df.sound$task == df.sound$task[i]),]$xmin) &  
      (df.sound$xmax[i] <= df.sound[(df.sound$dyad == df.sound$dyad[i]) &
                                    (df.sound$task == df.sound$task[i]),]$xmax)) > 1 ) { 
    df.sound$delete[i] = T
  } 
}
df.sound.engulfed = df.sound %>% filter(delete == T)
df.sound = df.sound %>% filter(delete == F) %>% select(-c(delete))

write_csv(df.sound.engulfed, file = file.path(dt.path[1], "OUT_engulfedsounds.csv"))

# identify turns: here, turns are defined as starting with the first sounding
# instance of a person until the end of the last sounding instance of this 
# person before a non-engulfed sounding instance of another person
df.out = df.sound %>%
  mutate(rown = row_number()) %>%             # add row number
  group_by(dyad, task, side) %>%                          # group by the person speaking
  mutate(
    tn = cumsum(c(TRUE, diff(rown) > 1))      # always keep the lowest row number of this turn as turn number
  ) %>%
  ungroup() %>%
  mutate(
    turn = paste0(side, tn)                   # add this turn number to the person speaking
  ) %>%
  group_by(dyad, task, side, turn) %>%        # summarise by dyad, task, side and turn
  summarise(
    start_turn = min(xmin, na.rm = T),        # take the start of the first sounding instance
    end_turn   = max(xmax, na.rm = T),        # take the end of the last sounding instance
    dur = end_turn - start_turn               # compute duration of the turn
  ) %>% 
  #select(-turn) %>%
  rename('speaker' = 'side') %>%
  arrange(dyad, task, start_turn) %>%
  group_by(dyad, task) %>%
  mutate(
    ttg = start_turn - lag(end_turn)
  )

write_csv(df.out, file = file.path(dt.path[1], "OUT_turns.csv"))

# Compute nsyl per turn ---------------------------------------------------

df.out = read_csv(file = file.path(dt.path[1], "OUT_turns.csv"), 
                  show_col_types = F)

# create an empty column for number of syllables in each turn
df.out = df.out %>%
  mutate(
    nsyl_turn = 0
  ) %>%
  arrange(dyad, task, start_turn)

# check if any silence files are missing
incomplete = c()
ls.fls = c() # list for all relevant files
for (d in ls.IDs) {
  file = file.path(dt.path[1], paste0(d, '_syllable.csv'))
  if (!file.exists(file)) {
    warning(sprintf('File %s does not exist', d))
    incomplete = c(incomplete, d)
  } else {
    ls.fls = c(ls.fls, file)
  }
}

# load all the data into one data frame
df  = do.call("rbind", lapply(ls.fls, read_csv, show_col_types = F))

# split filenames
df = df %>% 
  separate(col = Name, into = c("dyad1", "dyad2", "task", "ch", "side"), remove = T) %>%
  mutate(
    dyad = paste(dyad1, dyad2, sep = "_"),
    time = round(Number * 1000)
  ) %>%
  select(dyad, task, side, time)

# loop through the syllables and assign them to turns
for (j in 1:nrow(df)) {
  idx_turn = which(
    (df.out$start_turn <= df$time[j]) & 
    (df.out$end_turn   >= df$time[j]) &
    (df.out$dyad       == df$dyad[j]) &
    (df.out$task       == df$task[j])
  )
  if (length(idx_turn) == 1) {
    df.out$nsyl_turn[idx_turn] = df.out$nsyl_turn[idx_turn] + 1
  }
}

# calculate number of syllables per ms
df.out = df.out %>%
  mutate(
    nsyl_turn = na_if(nsyl_turn, 0), 
    art_turn  = (nsyl_turn * 1000) / dur # articulation rate for turn
  )

# Save output data frame --------------------------------------------------

write_csv(df.out, file.path(dt.path[1], "OUT_turns.csv"))
