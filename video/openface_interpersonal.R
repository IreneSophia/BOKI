# (C) Irene Sophia Plank
# 
# This script takes the output of OpenFace and calculates interpersonal 
# synchrony. It is an adaptation of a script written by Jana Koehler, 
# published in https://github.com/jckoe/MLASS-study. 

# clean workspace
rm(list = ls())

# load libraries
library(tidyverse)
library(rMEA)
library(data.table)    # setDT
library(moments)       # kurtosis, skewness

# set path to MEA files
dt.path = "/home/emba/Documents/ML_BOKI/Data_OpenFace"
setwd(dt.path)

# set options
options(datatable.fread.datatable = F)

# Read in data ------------------------------------------------------------

# lists all relevant IDs
ls.IDs = substr(list.files(path = dt.path, pattern = "BOKI_.*\\.csv"), 1, 11)

# check if there are dyads where one or more files are missing
incomplete = c()
for (d in unique(substr(ls.IDs, 1, 7))) {
  idx = which(substr(ls.IDs, 1, 7) == d)
  if (length(idx) != 4) {
    warning(sprintf('Dyad %s does not have 4 files', d))
    incomplete = c(incomplete, idx)
  }
}

# get rid of those IDs
ls.IDs = ls.IDs[-incomplete]

# list all relevant files
ls.fls = list.files(path = dt.path, pattern = "BOKI_.*\\.csv", full.names = T)
ls.fls = ls.fls[-incomplete]

# check if lists are in same order
for (i in 1:length(ls.fls)) { 
  if (substr(ls.fls[i], nchar(dt.path)+2, nchar(dt.path)+12) != ls.IDs[i]) {
    warning(ls.IDs[i])
  }
}
  
# reads in the data
col.drp = c(2,6:296,300:679) # irrelevant columns
ls      = lapply(ls.fls, fread, 
                 header = F, # preserves columns for frame number
                 skip = 301, # skips first 10 seconds (b/c of MEA artefacts)
                 # timestamp, confidence, success, position (pitch, yaw, roll), 
                 # intensity and occurrence of different AUs
                 drop = col.drp) 

# read in header separately 
header  = fread(ls.fls[1], header = F, nrows = 1, stringsAsFactors=F,  drop = col.drp)
for (i in 1:length(ls)){
  colnames(ls[[i]]) = unlist(header)
}

# add names to list elements
names(ls) = ls.IDs

# Inspect data ------------------------------------------------------------

# check for NAs 
for (i in 1:length(ls)){
  if (anyNA(ls[[i]]) == T){ 
    warning(sprintf('%s has NAs', names(ls)[i]))
  }
}

# check if every dataframe has 18,000 lines
for (i in 1:length(ls)){
  if (nrow(ls[[i]]) != 18000){ 
    warning(sprintf('%s is incomplete: %i', names(ls)[i], nrow(ls[[i]])))
  }
}

# filter data based on mean confidence and successfully tracked frames 
outlier = c()
for (i in 1:length(ls)) {
  if (mean(ls[[i]]$confidence) < 0.75 # mean confidence of tracked frames lower than 75%?
      || # OR
      sum(ls[[i]]$success) < 18000*0.9) # less than 90% of 18,000 successfully tracked?
  { 
    warning(sprintf('face tracking not reliable enough for %s', names(ls)[i]))
    if (i %% 2) {out.dyad = i} else {out.dyad = i-1}
    outlier = c(outlier, out.dyad) # add dyad numer to outliers
  }
}

# exclude outliers and their interaction partners
outlier = c(outlier, outlier+1)
if (length(outlier) > 0) {ls.clean = ls[-outlier]} else {ls.clean = ls}

# convert to dataframe
df = bind_rows(ls.clean, .id = "ID") %>%
  separate(col = ID, into = c("dyad1", "dyad2", "task", "speaker"), remove = F) %>%
  mutate(dyad = paste(dyad1, dyad2, sep = "_"),
         speaker = recode_factor(speaker, "R" = "BPD", "L" = "CTR"),
         ID = paste0(dyad, "_", speaker)) %>%
  select(-dyad1, -dyad2) %>%
  relocate(ID, dyad)

# clean workspace
rm(list = setdiff(ls(), "df"))

# list of AUs
ls.AUs = str_subset(names(df), "AU.*_r|pose_R*")

# Time series synchronisation ---------------------------------------------

# Steps: 
# 1) create fake MEA object
# 2) calculate ccf according to rMEA

# 1) initialize function to create a fake MEA object out of two vectors
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

# 2) calculate ccf according to rMEA
sampRate = 30
df.sync = data.frame()
for (i in unique(df$dyad)){ 
  
  # initialise heatmaps
  pdf(paste0("pics/", i, ".pdf")) 
  
  ## HOBBIES
  
  # grab only relevant portions of df
  df.sel = df[df$dyad == i & df$task == "H",] 
  
  # check if data frame is present
  if (nrow(df.sel) > 0) { 
    
    # loop over AUs
    for (j in ls.AUs){ 
      
      # prepare fake MEA components
      s1 = df.sel[df.sel$speaker == "CTR", j] # AU for left CTR participant
      s2 = df.sel[df.sel$speaker == "BPD", j] # AU for right BPD participant
      
      # create fake MEA object
      mea = fakeMEA(s1, s2, sampRate) 
      
      # time lagged windowed cross-correlations
      mea = MEAccf(mea, lagSec = 2, winSec = 7, incSec = 4, r2Z = T, ABS = T) 
      names(mea) = paste(i, "H", j, sep = "_")
      
      # extract matrix with all ccf values over all lags and windows 
      df.ccf = mea[[1]][["ccf"]] 
      
      # configure heatmap
      par(col.main='white')                  # set plot title to white
      heatmap = MEAheatmap(mea[[1]])
      par(col.main='black')                  # set plot title back to black
      title(main = paste("H", j, sep = "_")) # alternative title
      
      # peak picking
      CTR = apply(df.ccf[,c(1:60)], 1, max, na.rm =T ) 
      BPD = apply(df.ccf[,c(62:121)], 1, max, na.rm =T )
      df.dyad = as.data.frame(cbind(CTR,BPD)) %>%
        rownames_to_column(var = "window") %>%
        mutate(
          dyad = i,
          task = "H",
          input = j
        ) %>%
        pivot_longer(cols = c("CTR", "BPD"), names_to = "speaker", values_to = "sync") %>%
        mutate(
          sync = if_else(sync != -Inf & !is.na(sync), sync, NA)
        )
      
      df.sync = rbind(df.sync, df.dyad)
    }
  }
  
  ## MEALPLANNING
  
  # grab only relevant portions of df
  df.sel = df[df$dyad == i & df$task == "M",] 
  
  # check if data frame is present
  if (nrow(df.sel) > 0) { 
    
    # loop over AUs
    for (j in ls.AUs){ 
      
      # prepare fake MEA components
      s1 = df.sel[df.sel$speaker == "CTR", j] # AU for left CTR participant
      s2 = df.sel[df.sel$speaker == "BPD", j] # AU for right BPD participant
      
      # create fake MEA object
      mea = fakeMEA(s1, s2, sampRate) 
      
      # time lagged windowed cross-correlations
      mea = MEAccf(mea, lagSec = 2, winSec = 7, incSec = 4, r2Z = T, ABS = T) 
      names(mea) = paste(i, "M", j, sep = "_")
      
      # extract matrix with all ccf values over all lags and windows 
      df.ccf = mea[[1]][["ccf"]] 
      
      # configure heatmap
      par(col.main='white')                  # set plot title to white
      heatmap = MEAheatmap(mea[[1]])
      par(col.main='black')                  # set plot title back to black
      title(main = paste("H", j, sep = "_")) # alternative title
      
      # peak picking
      CTR = apply(df.ccf[,c(1:60)], 1, max, na.rm =T ) 
      BPD = apply(df.ccf[,c(62:121)], 1, max, na.rm =T )
      df.dyad = as.data.frame(cbind(CTR,BPD)) %>%
        rownames_to_column(var = "window") %>%
        mutate(
          dyad = i,
          task = "M",
          input = j
        ) %>%
        pivot_longer(cols = c("CTR", "BPD"), names_to = "speaker", values_to = "sync") %>%
        mutate(
          sync = if_else(sync != -Inf & !is.na(sync), sync, NA)
        )
      
      df.sync = rbind(df.sync, df.dyad)
    }
  }
  
  dev.off()
  # show progress
  print(paste(i, "done"))
}

# clean workspace
rm(list = setdiff(ls(), c("df", "df.sync")))

# check missing values for each AU and pose
df.AUs = df.sync %>%
  group_by(dyad, speaker, input, task) %>%
  summarise(
    missing = sum(is.na(sync)) / n()
  ) %>%
  group_by(input) %>%
  summarise(
    missing = max(missing)
  ) %>% 
  filter(missing < 0.5)
ls.AUs = unique(df.AUs$input) # list of all AUs that should be included

# calculate summary statistics
df.OFsync = df.sync %>%
  group_by(dyad, speaker, input, task) %>%
  summarise(
    OF.sync.min  = min(sync, na.rm = T),
    OF.sync.max  = max(sync, na.rm = T),
    OF.sync.sd   = sd(sync, na.rm = T),
    OF.sync.mean = mean(sync, na.rm = T),
    OF.sync.md   = median(sync, na.rm = T),
    OF.sync.kurt = kurtosis(sync, na.rm = T),
    OF.sync.skew = skewness(sync, na.rm = T)
  ) %>%
  mutate(
    ID = paste0(dyad, "_", speaker)
  ) %>%
  relocate(ID) %>%
  filter(input %in% ls.AUs) # only keep AUs where no participant was missing more than 50% 
ls.features = str_subset(names(df.OFsync), pattern = "OF.sync.*")

df.OFsync_NM = df.OFsync %>%
  pivot_wider(names_from = c(task, input), values_from = all_of(ls.features))

write.csv(df.OFsync_NM, "FE_syncentrain.csv")

# clean workspace
rm(list = setdiff(ls(), c("df", "df.sync", "df.OFsync_NM")))

# Facial expressiveness ---------------------------------------------------

# operationalised as the mean intensity of all included AUs per task
df.OFexp_NM = df %>%
  select(ID, dyad, task, speaker, frame, matches("AU.*r")) %>%
  pivot_longer(names_to = "input", values_to = "exp", cols = matches("AU.*r")) %>%
  group_by(ID, dyad, task, speaker) %>%
  summarise(
    exp = mean(exp, na.rm = T)
  ) %>%
  pivot_wider(names_from = task, values_from = exp, names_prefix = "exp_")

write.csv(df.OFexp_NM, "FE_intensity.csv")

# Save workspace ----------------------------------------------------------

save.image(file = "OpenFace.Rdata")
