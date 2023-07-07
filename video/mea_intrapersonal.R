# (C) Irene Sophia Plank
# 
# This script takes the output of Motion Energy analysis as well as OpenFace and 
# calculates INTRApersonal synchrony. It is an adaptation of a script written by 
# Jana Koehler, published in https://github.com/jckoe/MLASS-study. 

# BPD participants always sit on the right, CTR on the left. In MEA, the order
# of the ROI is: CTR_head, CTR_body, BPD_head, BPD_body

# clean workspace
rm(list = ls())

# load libraries
library(tidyverse)
library(rMEA)
library(data.table)    # setDT
library(moments)       # kurtosis, skewness
library(filesstrings)  # file.move

# set path to partent directory of MEA and BOKI files
dt.path = "/home/emba/Documents/ML_BOKI/"
setwd(dt.path)

# set options
options(datatable.fread.datatable = F)

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

# Read in and clean data from OpenFace ------------------------------------

# lists all relevant IDs
ls.IDs = substr(list.files(path = paste0(getwd(), "/Data_OpenFace/"), pattern = "BOKI_.*\\.csv"), 1, 11)

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
ls.fls = list.files(path = paste0(getwd(), "/Data_OpenFace/"), pattern = "BOKI_.*\\.csv", full.names = T)
ls.fls = ls.fls[-incomplete]

# check if lists are in same order
l = nchar(paste0(getwd(), "/Data_OpenFace/"))
for (i in 1:length(ls.fls)) { 
  if (substr(ls.fls[i], l + 2, l + 12) != ls.IDs[i]) {
    warning(ls.IDs[i])
  }
}

# reads in the data
col.sel = c(1,3:5,294:296) # irrelevant columns
ls      = lapply(ls.fls, fread, 
                 header = F, # preserves columns for frame number
                 skip = 300, # skips first 10 seconds (b/c of MEA artefacts) -> one less due to diff!
                 # timestamp, confidence, success, position (pitch, yaw, roll), 
                 # intensity and occurrence of different AUs
                 select = col.sel) 

# read in header separately 
header  = fread(ls.fls[1], header = F, nrows = 1, stringsAsFactors = F,  select = col.sel)
for (i in 1:length(ls)){
  colnames(ls[[i]]) = unlist(header)
}

# add names to list elements
names(ls) = ls.IDs

# check for NAs 
for (i in 1:length(ls)){
  if (anyNA(ls[[i]]) == T){ 
    warning(sprintf('%s has NAs', names(ls)[i]))
  }
}

# check if every dataframe has 18,001 lines
for (i in 1:length(ls)){
  if (nrow(ls[[i]]) != 18001){ 
    warning(sprintf('%s is incomplete: %i', names(ls)[i], nrow(ls[[i]])))
  }
}

# filter data based on mean confidence and successfully tracked frames 
outlier = c()
for (i in 1:length(ls)) {
  if (mean(ls[[i]]$confidence) < 0.75 # mean confidence of tracked frames lower than 75%?
      || # OR
      sum(ls[[i]]$success) < 18001*0.9) # less than 90% of 18,000 successfully tracked?
  { 
    warning(sprintf('face tracking not reliable enough for %s', names(ls)[i]))
    if (i %% 2) {out.dyad = i} else {out.dyad = i-1}
    outlier = c(outlier, out.dyad) # add dyad numer to outliers
  }
}

# exclude outliers and their interaction partners
outlier = c(outlier, outlier+1)
if (length(outlier) > 0) {ls.clean = ls[-outlier]} else {ls.clean = ls}

# Create head movement vector ---------------------------------------------

# initialize function
vlength = function(x) {
  length = sqrt((x[1])^2+(x[2])^2+(x[3])^2)
  return(length)
}

# loop through list of dataframes 
for (i in 1:length(ls.clean)){
  # calculate frame-to-frame differences
  ls.clean[[i]]$diff_Tx = c(NA,diff(ls.clean[[i]]$pose_Tx)) 
  ls.clean[[i]]$diff_Ty = c(NA,diff(ls.clean[[i]]$pose_Ty))
  ls.clean[[i]]$diff_Tz = c(NA,diff(ls.clean[[i]]$pose_Tz))
  # delete first row of NA
  ls.clean[[i]] = ls.clean[[i]][-1,]  
  # calculate vector length
  ls.clean[[i]]$OF.headmov = apply(ls.clean[[i]][,c("diff_Tx","diff_Ty","diff_Tz")],1,vlength)
}

# convert to dataframe
df.OF = bind_rows(ls.clean, .id = "ID") %>%
  separate(col = ID, into = c("dyad1", "dyad2", "task", "speaker"), remove = F) %>%
  mutate(dyad = paste(dyad1, dyad2, sep = "_"),
         speaker = recode_factor(speaker, "R" = "BPD", "L" = "CTR"),
         ID = paste0(dyad, "_", speaker)) %>%
  select(-dyad1, -dyad2) %>%
  select(ID, dyad, task, speaker, frame, OF.headmov)

# clean workspace
rm(list = setdiff(ls(), c("df.OF", "fakeMEA")))

# Read in body movement from MEA ------------------------------------------

df.MEA = list.files(path = paste0(getwd(), "/Data_MEA"), 
                    pattern = "BOKI_.*\\.txt", full.names = T) %>%
  setNames(nm = .) %>%
  map_df(~read_delim(.x, show_col_types = F, skip = 300,
                     col_names = F, col_select = c(2,4)), 
         .id = "ID") %>% 
  rename("CTR" = "X2", "BPD" = "X4") %>%
  mutate(
    ID = gsub(".txt", "", gsub(paste0(getwd(), "/Data_MEA/"), "", ID))
  ) %>% 
  group_by(ID) %>%
  mutate(
    frame = row_number()+300
  ) %>%
  separate(col = ID, into = c("dyad1", "dyad2", "task"), remove = F) %>%
  mutate(
    dyad = paste(dyad1, dyad2, sep = "_")
    ) %>%
  select(-dyad1, -dyad2) %>%
  pivot_longer(names_to = "speaker", values_to = "MEA.bodymov", cols = c("CTR", "BPD")) %>%
  mutate(
    ID = paste0(dyad, "_", speaker)
  )

# check which participants have the wrong number of frames
df.MEA %>%
  group_by(ID, task) %>%
  summarise(
    min = min(frame),
    max = max(frame), 
    count = n()
  ) %>%
  filter(count != 18000)

# clean workspace
rm(list = setdiff(ls(), c("df.OF", "df.MEA", "fakeMEA")))

# Calculate intrapersonal synchrony ---------------------------------------

# merge both dataframes
df = merge(df.OF, df.MEA)

# loop through participants
sampRate = 30
df.IA.sync = data.frame()# initialise heatmaps
pdf(paste0(getwd(), "/Data_mixed/heatmaps_intra.pdf")) 
for (i in unique(df$ID)){
  
  ## HOBBIES
  
  # grab only relevant portions of df
  df.sel = df %>%
    filter(ID == i & task == "H") %>%
    arrange(ID, dyad, task, speaker, frame)
  
  # check if data frame is present
  if (nrow(df.sel) > 0) { 
    
    # prepare fake MEA components
    s1 = df.sel$MEA.bodymov # body movement
    s2 = df.sel$OF.headmov  # head movement
    
    # create fake MEA object
    mea = fakeMEA(s1, s2, sampRate) 
    
    # time lagged windowed cross-correlations
    mea = MEAccf(mea,lagSec = 5, winSec = 30, incSec = 15, r2Z = T, ABS = T) 
    names(mea) = paste(i, "H", sep = "_")
    
    # extract matrix with all ccf values over all lags and windows 
    df.ccf = mea[[1]][["ccf"]] 
    
    # configure heatmap
    par(col.main='white')                  # set plot title to white
    heatmap = MEAheatmap(mea[[1]])
    par(col.main='black')                  # set plot title back to black
    title(main = paste(i, "H", sep = "_")) # alternative title
    
    # peak picking
    sync = apply(df.ccf, 1, max, na.rm =T ) 
    
    df.indi = as.data.frame(sync) %>%
      rownames_to_column(var = "window") %>%
      mutate(
        ID = i,
        task = "H"
      ) %>%
      mutate(
        sync = if_else(sync != -Inf & !is.na(sync), sync, NA)
      )
    
    df.IA.sync = rbind(df.IA.sync, df.indi)
  }
  
  ## MEALPLANNING
  
  # grab only relevant portions of df
  df.sel = df %>%
    filter(ID == i & task == "M") %>%
    arrange(ID, dyad, task, speaker, frame)
  
  # check if data frame is present
  if (nrow(df.sel) > 0) { 
    
    # prepare fake MEA components
    s1 = df.sel$MEA.bodymov # body movement
    s2 = df.sel$OF.headmov  # head movement
    
    # create fake MEA object
    mea = fakeMEA(s1, s2, sampRate) 
    
    # time lagged windowed cross-correlations
    mea = MEAccf(mea,lagSec = 5, winSec = 30, incSec = 15, r2Z = T, ABS = T) 
    names(mea) = paste(i, "M", sep = "_")
    
    # extract matrix with all ccf values over all lags and windows 
    df.ccf = mea[[1]][["ccf"]] 
    
    # configure heatmap
    par(col.main='white')                  # set plot title to white
    heatmap = MEAheatmap(mea[[1]])
    par(col.main='black')                  # set plot title back to black
    title(main = paste(i, "M", sep = "_")) # alternative title
    
    # peak picking
    sync = apply(df.ccf, 1, max, na.rm =T ) 
    
    df.indi = as.data.frame(sync) %>%
      rownames_to_column(var = "window") %>%
      mutate(
        ID = i,
        task = "M"
      ) %>%
      mutate(
        sync = if_else(sync != -Inf & !is.na(sync), sync, NA)
      )
    
    df.IA.sync = rbind(df.IA.sync, df.indi)
  }
  
  # show progress
  print(paste(i, "done"))
}
dev.off()

# calculate summary statistics
df.IA.sync_NM = df.IA.sync %>%
  group_by(ID, task) %>%
  summarise(
    IA.sync_min  = min(sync, na.rm = T),
    IA.sync_max  = max(sync, na.rm = T),
    IA.sync_sd   = sd(sync, na.rm = T),
    IA.sync_mean = mean(sync, na.rm = T),
    IA.sync_md   = median(sync, na.rm = T),
    IA.sync_kurt = kurtosis(sync, na.rm = T),
    IA.sync_skew = skewness(sync, na.rm = T)
  ) %>%
  mutate(
    dyad = substr(ID, 1, 7),
    speaker = substr(ID, 8, 11)
  ) %>%
  relocate(ID, dyad, speaker) %>%
  pivot_wider(names_from = "task", values_from = where(is.numeric))

# save as csv
write.csv(df.IA.sync_NM,"peaks_intra.csv")

# clean workspace
rm(list = setdiff(ls(), c("df", "df.OF", "df.MEA", "df.IA.sync", "df.IA.sync_NM")))

# Save workspace ----------------------------------------------------------

# save workspace
save.image(paste0(getwd(), "/Data_mixed/intra.RData"))
