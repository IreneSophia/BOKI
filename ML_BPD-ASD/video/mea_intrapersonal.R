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

# set path to MEA files
dt.path = c("/media/emba/emba-2/ML_BOKI/MEA_preprocessed", 
            "/media/emba/emba-2/ML_BOKI/OpenFace_preprocessed", 
            "/media/emba/emba-2/ML_BOKI/ML_data")

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
df.sub = read_csv(file.path("/media/emba/emba-2/ML_BOKI/demoCentraXX", 
                            "BOKI_centraXX.csv")) %>%
  filter(substr(dyad, 1, 4) == "BOKI") %>%
  select(dyad, ID, label)
ls.inc = unique(df.sub$dyad)
ls.IDs = c(paste0(ls.inc, "_H_L"), paste0(ls.inc, "_H_R"),
           paste0(ls.inc, "_M_L"), paste0(ls.inc, "_M_R"))

# check if any files are missing
incomplete = c()
ls.fls = c() # list for all relevant files
for (d in ls.IDs) {
  file = file.path(dt.path[2], paste0(d, '.csv'))
  if (!file.exists(file)) {
    warning(sprintf('File %s does not exist', d))
    incomplete = c(incomplete, d)
  } else {
    ls.fls = c(ls.fls, file)
  }
}

# reads in the data
ls      = lapply(ls.fls, fread, 
                 header = F, # preserves columns for frame number
                 skip = 300, # skips first 10 seconds (b/c of MEA artefacts),
                 nrows = 18001 # 10 minutes * 30 frames * 60 to convert to seconds
                 # timestamp, confidence, success, position (pitch, yaw, roll), 
                 # intensity and occurrence of different AUs
                 ) 

# read in header separately 
header  = fread(ls.fls[1], header = F, nrows = 1, stringsAsFactors = F)
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

# check if every dataframe has 18,001 lines > one is lost later due to diff
for (i in 1:length(ls)){
  if (nrow(ls[[i]]) != 18001){ 
    warning(sprintf('%s is incomplete: %i', names(ls)[i], nrow(ls[[i]])))
  }
}

# Create head movement vector ---------------------------------------------

# initialize function
vlength = function(x) {
  length = sqrt((x[1])^2+(x[2])^2+(x[3])^2)
  return(length)
}

# loop through list of dataframes 
for (i in 1:length(ls)){
  # calculate frame-to-frame differences
  ls[[i]]$diff_Tx = c(NA,diff(ls[[i]]$pose_Tx)) 
  ls[[i]]$diff_Ty = c(NA,diff(ls[[i]]$pose_Ty))
  ls[[i]]$diff_Tz = c(NA,diff(ls[[i]]$pose_Tz))
  # delete first row of NA
  ls[[i]] = ls[[i]][-1,]  
  # calculate vector length
  ls[[i]]$OF.headmov = apply(ls[[i]][,c("diff_Tx","diff_Ty","diff_Tz")],1,vlength)
}

# convert to dataframe
df.OF = bind_rows(ls, .id = "ID") %>%
  separate(col = ID, into = c("dyad1", "dyad2", "task", "speaker"), remove = F) %>%
  mutate(dyad = paste(dyad1, dyad2, sep = "_"),
         ID = paste0(dyad, "_", speaker)) %>%
  select(-dyad1, -dyad2) %>%
  select(ID, dyad, task, speaker, frame, OF.headmov)

# clean workspace
rm(list = setdiff(ls(), c("df.OF", "fakeMEA", "dt.path", "df.sub")))

# Read in body movement from MEA ------------------------------------------

df.MEA = list.files(path = dt.path[1], 
                    pattern = "BOKI_.*\\.txt", full.names = T) %>%
  setNames(nm = .) %>%
  map_df(~read_delim(.x, show_col_types = F, skip = 300, n_max = 18000,
                     col_names = F, col_select = c(2,4)), 
         .id = "ID") %>% 
  rename("L" = "X2", "R" = "X4") %>%
  mutate(
    ID = gsub(".*preprocessed/(.+).txt", "\\1", ID)
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
  filter(dyad %in% unique(df.OF$dyad)) %>%
  pivot_longer(names_to = "speaker", values_to = "MEA.bodymov", cols = c("L", "R")) %>%
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
rm(list = setdiff(ls(), c("df.OF", "df.MEA", "fakeMEA", "dt.path", "df.sub")))

# Calculate intrapersonal synchrony ---------------------------------------

# create list to be filled with fakeMEA objects
ls.mea = c()

# merge both dataframes
df = merge(df.OF, df.MEA)

# save the dataframe
saveRDS(df, file.path(dt.path[1], "BOKI_intra.rds"))

# loop through participants
sampRate = 30
df.IA.sync = data.frame()# initialise heatmaps
pdf(file.path(dt.path[1], "heatmaps_intra.pdf")) 

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
    
    # add object to fakeMEA list
    ls.mea = c(ls.mea, mea)
    
    # extract matrix with all ccf values over all lags and windows 
    df.ccf = mea[[1]][["ccf"]] 
    
    # configure heatmap
    par(col.main='white')                  # set plot title to white
    heatmap = MEAheatmap(mea[[1]])
    par(col.main='black')                  # set plot title back to black
    title(main = paste(i, "H", sep = "_")) # alternative title
    
    # peak picking
    sync = apply(df.ccf, 1, max, na.rm = T ) 
    
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
    
    # add object to fakeMEA list
    ls.mea = c(ls.mea, mea)
    
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
    min  = min(sync, na.rm = T),
    max  = max(sync, na.rm = T),
    sd   = sd(sync, na.rm = T),
    mean = mean(sync, na.rm = T),
    md   = median(sync, na.rm = T),
    kurtosis = kurtosis(sync, na.rm = T),
    skew = skewness(sync, na.rm = T)
  ) %>%
  mutate(
    dyad = substr(ID, 1, 7),
    speaker = substr(ID, 9, 9)
  ) %>%
  pivot_wider(names_from = "task", values_from = where(is.numeric),
              names_glue = "{.value}_{task}_intra") %>%
  merge(., df.sub %>% select(ID, label)) %>%
  relocate(ID, dyad, speaker, label)

# save as csv
write_csv(df.IA.sync_NM, file.path(dt.path[3], "BOKI_peaks_intra_NM.csv"))

# clean workspace
rm(list = setdiff(ls(), c("df", "df.OF", "df.MEA", "df.IA.sync", 
                          "df.IA.sync_NM", "ls.mea", "dt.path")))

# Save workspace ----------------------------------------------------------

# save workspace
save.image(file.path(dt.path[1], "intra.RData"))

