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

# set path to files
dt.path = c("/media/emba/emba-2/ML_BOKI/OpenFace_preprocessed", 
            "/media/emba/emba-2/ML_BOKI/AUD_preprocessed", 
            "/media/emba/emba-2/ML_BOKI/MEA_preprocessed", 
            "/media/emba/emba-2/ML_BOKI/MLASS_txtfiles", 
            "/media/emba/emba-2/ML_BOKI/ML_data")

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

# Read in Data ------------------------------------------------------------

# MEA DATA

# initialise dataframe
df.mea = data.frame()

# BOKI stuff
load(file.path(dt.path[3], 'MEA.Rdata'))
rm(list = c('df.MEA.body_NM', 'df.MEA.head_NM', 'df.MEA.mov_NM', 'df.ccf'))

for (i in 1:length(mea.ccf)) {
  L = as.numeric(mea.ccf[[i]]$MEA$L)
  R = as.numeric(mea.ccf[[i]]$MEA$R)
  info = str_split(names(mea.ccf[i]), pattern = "_")
  dyad = rep(sprintf("BOKI_%s", info[[1]][2]), times = 18000)
  task = rep(info[[1]][3], times = 18000)
  ROI  = rep(info[[1]][1], times = 18000)
  frame = 1:18000
  df.mea = rbind(df.mea, data.frame(dyad, task, ROI, frame, L, R))
}
rm("mea.ccf")

# add the MLASS stuff


# OPEN FACE DATA

df.MLASS = readRDS(file.path(dt.path[1], "MLASS_OF.rds")) %>%
  rename("diagnosis" = "dx", "speaker" = "position") %>%
  mutate(
    task  = case_match(task, "hobbies" ~ "H", "mealplanning" ~ "M"),
    label = case_match(diagnosis, "TD" ~ 0, "ASD" ~ 1)
  ) %>%
  group_by(dyad) %>%
  mutate(
    label = sum(label),
    label = case_when(
      label > 0  ~ "ASD-COMP",
      label == 0 ~ "COMP-COMP"
    )
  )

df.OF = readRDS(file.path(dt.path[1], "BOKI_OF.rds")) %>% select(-c(face_id, pose_Tx, pose_Ty, pose_Tz)) %>%
  rbind(., df.MLASS)

# SPEECH DATA

df.turns = read_csv(file.path(dt.path[2], "OUT_turns.csv"))

# df.AUD = list.files(path = dt.path[2], pattern = "BOKI_08.*_cont.csv") %>%
#   setNames(nm = .) %>%
#   map_df(~read_csv(file.path(dt.path[2],.), col_types = "ddd", show_col_types = F), .id = "fln") %>%
#   mutate(
#     dyad = substr(fln, 1,7),
#     task = substr(fln, 9,9),
#     speaker = substr(fln, 14,14),
#     time = time * 1000 # convert to ms
#     ) %>% select(-fln)

# read in and downsample the continuous data to 120Hz
df.AUD = data.frame()
ls.AUD = list.files(path = dt.path[2], pattern = ".*_cont.csv")
for (i in ls.AUD) {
  df.temp = read_csv(file.path(dt.path[2],i), col_types = "ddd", show_col_types = F)
  pitch = as.numeric(df.temp$pitch)
  if (substr(i, 1, 4) == "BOKI") {
    df.temp = df.temp %>%
      mutate(
        dyad = substr(i, 1,7),
        task = substr(i, 9,9),
        speaker = substr(i, 14,14),
        time = time * 1000 # convert to ms
      )
  } else {
    
  }
}
df.AUD = list.files(path = dt.path[2], pattern = "BOKI_.*_cont.csv") %>%
  setNames(nm = .) %>%
  map_df(~read_csv(file.path(dt.path[2],.), col_types = "ddd", show_col_types = F), .id = "fln") %>%
  mutate(
    dyad = substr(fln, 1,7),
    task = substr(fln, 9,9),
    speaker = substr(fln, 14,14),
    time = time * 1000 # convert to ms
  ) %>% select(-fln)


# Add turn to MEA data ----------------------------------------------------



# Add turn to OF data -----------------------------------------------------

if (!file.exists(file.path(dt.path[1], "BOKI_OF_turns.rds"))) {
  df.OF = df.OF %>%
    mutate(
      time = (timestamp - 10)*1000 + 1,
      turn_self  = 0,
      turn_other = 0
    ) %>%
    select(-frame, -timestamp) %>%
    relocate(dyad, label, ID, task, speaker, diagnosis, time, turn_self, turn_other)
  
  for (i in 1:nrow(df.turns)) { #
    print(i)
    d = df.turns$dyad[i]
    s = df.turns$speaker[i]
    t = df.turns$task[i]
    xmin = df.turns$start_turn[i]
    xmax = df.turns$end_turn[i]
    df.OF[df.OF$dyad == d & df.OF$task == t & df.OF$speaker == s & df.OF$time <= xmax & df.OF$time >= xmin,]$turn_self  = 1
    df.OF[df.OF$dyad == d & df.OF$task == t & df.OF$speaker != s & df.OF$time <= xmax & df.OF$time >= xmin,]$turn_other = 1
  }
  
  saveRDS(df.OF, file = file.path(dt.path[1], "BOKI_OF_turns.rds"))
  
} else {
  df.OF = readRDS(file.path(dt.path[1], "BOKI_OF_turns.rds"))
}

