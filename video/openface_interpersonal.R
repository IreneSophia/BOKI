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

# set path to OpenFace files
dt.path = c("/media/emba/emba-2/ML_BOKI/OpenFace_preprocessed", 
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

# Read in data ------------------------------------------------------------

if (!file.exists(file.path(dt.path[1], "BOKI_OF.rds"))) {
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
    file = file.path(dt.path[1], paste0(d, '.csv'))
    if (!file.exists(file)) {
      warning(sprintf('File %s does not exist', d))
      incomplete = c(incomplete, d)
    } else {
      ls.fls = c(ls.fls, file)
    }
  }
  
  # no incomplete dyads
  
  # reads in the data
  #col.drp = c(2,6:296,300:679) # irrelevant columns
  ls      = lapply(ls.fls, fread, 
                   header = F, # preserves columns for frame number
                   skip = 301, # skips first 10 seconds (b/c of MEA artefacts) + header
                   nrows = 18000 # 10 minutes * 30 frames * 60 to convert to seconds
                   # timestamp, confidence, success, position (pitch, yaw, roll), 
                   # intensity and occurrence of different AUs
                   #drop = col.drp
  ) 
  
  # read in header separately 
  header  = fread(ls.fls[1], header = F, nrows = 1, stringsAsFactors = F)#,  drop = col.drp)
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
  
  # there are no outliers that need to be excluded
  
  # convert to dataframe
  df = bind_rows(ls, .id = "ID") %>%
    separate(col = ID, into = c("dyad1", "dyad2", "task", "speaker"), remove = F) %>%
    mutate(dyad = paste(dyad1, dyad2, sep = "_"),
           ID = paste0(dyad, "_", speaker)) %>%
    select(-dyad1, -dyad2) %>%
    merge(., df.sub %>% select(ID, label)) %>%
    mutate(
      diagnosis = case_when(
        label == "COMP-COMP" ~ "COMP", 
        speaker == "L" ~ "COMP", 
        speaker == "R" ~ "BPD"
      )
    ) %>%
    relocate(ID, dyad, speaker, label, diagnosis, task)
  
  saveRDS(df, file.path(dt.path[1], "BOKI_OF.rds"))
} else {
  df = readRDS(file.path(dt.path[1], "BOKI_OF.rds"))
}

# clean workspace
rm(list = setdiff(ls(), c("df", "fakeMEA", "dt.path")))

# list of AUs
ls.AUs = str_subset(names(df), "AU.*_r|pose_R*")

# remove some based on Koehler et al. (2024), Translational Psychiatry
ls.AUs.M = setdiff(ls.AUs, c("AU04_r", "AU05_r", "AU10_r", "AU12_r", "AU23_r"))
ls.AUs.H = setdiff(ls.AUs, c("AU04_r", "AU05_r", "AU10_r", "AU12_r", "AU14_r"))

# Time series synchronisation ---------------------------------------------

if (!file.exists(file.path(dt.path[1], "BOKI_sync.RData"))) {
  
  # Steps: 
  # 1) create fake MEA object using the fakeMEA function
  # 2) calculate ccf according to rMEA
  
  # create list to be filled with fakeMEA objects of AUs
  ls.fakeAU = c()
  
  # loop through all dyads
  sampRate = 30
  df.AU.sync = data.frame()
  for (i in unique(df$dyad)){ 
    
    # initialise heatmaps
    dir.create(file.path(dt.path[1], 'pics'), showWarnings = FALSE)
    pdf(file.path(dt.path[1], 'pics', paste0(i, ".pdf")))
    
    ## HOBBIES
    
    # grab only relevant portions of df
    df.sel = df %>%
      filter(dyad == i & task == "H") %>%
      arrange(ID, dyad, task, speaker, diagnosis, frame)
    
    # check if data frame is present
    if (nrow(df.sel) > 0) { 
      
      # loop over AUs
      for (j in ls.AUs.H){ 
        
        # prepare fake MEA components
        s1 = df.sel[df.sel$speaker == "L", j] # AU for left L participant
        s2 = df.sel[df.sel$speaker == "R", j] # AU for right R participant
        
        # create fake MEA object
        mea = fakeMEA(s1, s2, sampRate) 
        
        # time lagged windowed cross-correlations
        mea = MEAccf(mea, lagSec = 2, winSec = 7, incSec = 4, r2Z = T, ABS = T) 
        names(mea) = paste(i, "H", j, sep = "_")
        
        # add object to fakeMEA list
        ls.fakeAU = c(ls.fakeAU, mea)
        
        # extract matrix with all ccf values over all lags and windows 
        df.ccf = mea[[1]][["ccf"]] 
        
        # configure heatmap
        par(col.main='white')                  # set plot title to white
        heatmap = MEAheatmap(mea[[1]])
        par(col.main='black')                  # set plot title back to black
        title(main = paste("H", j, sep = "_")) # alternative title
        
        # peak picking
        L = apply(df.ccf[,1:floor(ncol(df.ccf)/2)], 1, max, na.rm =T ) 
        R = apply(df.ccf[,(floor(ncol(df.ccf)/2)+2):ncol(df.ccf)], 1, max, na.rm =T )
        df.dyad = as.data.frame(cbind(L,R)) %>%
          rownames_to_column(var = "window") %>%
          mutate(
            dyad = i,
            task = "H",
            input = j
          ) %>%
          pivot_longer(cols = c("L", "R"), names_to = "speaker", values_to = "sync") %>%
          mutate(
            sync = if_else(sync != -Inf & !is.na(sync), sync, NA)
          )
        
        df.AU.sync = rbind(df.AU.sync, df.dyad)
      }
    }
    
    ## MEALPLANNING
    
    # grab only relevant portions of df
    df.sel = df %>%
      filter(dyad == i & task == "M") %>%
      arrange(ID, dyad, task, speaker, diagnosis, frame)
    
    # check if data frame is present
    if (nrow(df.sel) > 0) { 
      
      # loop over AUs
      for (j in ls.AUs.M){ 
        
        # prepare fake MEA components
        s1 = df.sel[df.sel$speaker == "L", j] # AU for left L participant
        s2 = df.sel[df.sel$speaker == "R", j] # AU for right R participant
        
        # create fake MEA object
        mea = fakeMEA(s1, s2, sampRate) 
        
        # time lagged windowed cross-correlations
        mea = MEAccf(mea, lagSec = 2, winSec = 7, incSec = 4, r2Z = T, ABS = T) 
        names(mea) = paste(i, "M", j, sep = "_")
        
        # add object to fakeMEA list
        ls.fakeAU = c(ls.fakeAU, mea)
        
        # extract matrix with all ccf values over all lags and windows 
        df.ccf = mea[[1]][["ccf"]] 
        
        # configure heatmap
        par(col.main='white')                  # set plot title to white
        heatmap = MEAheatmap(mea[[1]])
        par(col.main='black')                  # set plot title back to black
        title(main = paste("M", j, sep = "_")) # alternative title
        
        # peak picking
        L = apply(df.ccf[,1:floor(ncol(df.ccf)/2)], 1, max, na.rm =T ) 
        R = apply(df.ccf[,(floor(ncol(df.ccf)/2)+2):ncol(df.ccf)], 1, max, na.rm =T )
        df.dyad = as.data.frame(cbind(L,R)) %>%
          rownames_to_column(var = "window") %>%
          mutate(
            dyad = i,
            task = "M",
            input = j
          ) %>%
          pivot_longer(cols = c("L", "R"), names_to = "speaker", values_to = "sync") %>%
          mutate(
            sync = if_else(sync != -Inf & !is.na(sync), sync, NA)
          )
        
        df.AU.sync = rbind(df.AU.sync, df.dyad)
      }
    }
    
    dev.off()
    # show progress
    print(paste(i, "done"))
  }
  
  save(df.AU.sync, ls.fakeAU, file = file.path(dt.path[1], "BOKI_sync.RData"))
  
} else {
  
  load(file.path(dt.path[1], "BOKI_sync.RData"))
  
}

# clean workspace
rm(list = setdiff(ls(), c("df", "df.AU.sync", "fakeMEA", "ls.fakeAU", 
                          "dt.path", "ls.AUs.M", "ls.AUs.H")))

# calculate summary statistics
df.AU.sync_NM = df.AU.sync %>%
  filter(!grepl("pose_T", input)) %>%
  mutate(
    input = case_when(
      grepl("pose_R", input) ~ paste0(input, "sync"),
      T ~ input
      )
  ) %>% 
  group_by(dyad, speaker, input, task) %>%
  summarise(across(where(is.numeric), 
                         .fns = 
                           list(min  = ~min(.,na.rm = T), 
                                max  = ~max(.,na.rm = T), 
                                md   = ~median(.,na.rm = T), 
                                mean = ~mean(.,na.rm = T), 
                                sd   = ~sd(.,na.rm = T), 
                                kurtosis = ~kurtosis(.,na.rm = T), 
                                skew = ~skewness(.,na.rm = T)
                                ), .names = "{.fn}")
            ) %>%
  pivot_wider(names_from = c(task, input), values_from = where(is.numeric),
              names_glue = "{.value}_{task}_{input}") %>%
  mutate(
    ID = paste0(dyad, "_", speaker)
  ) %>%
  merge(., df %>% select(ID, label) %>% distinct()) %>%
  relocate(ID, dyad, speaker, label)

# save to csv file
write_csv(df.AU.sync_NM, file.path(dt.path[2], "BOKI_FE_syncentrain_NM.csv"))

# clean workspace
rm(list = setdiff(ls(), c("df", "df.AU.sync", "df.AU.sync_NM", "fakeMEA", 
                          "ls.fakeAU", "dt.path", "ls.AUs.M", "ls.AUs.H")))

# Facial expressiveness ---------------------------------------------------

# create list to be filled with fakeMEA objects of facial expressiveness
ls.fakeFE = c()

if (!file.exists(file.path(dt.path[1], "BOKI_exp.RData"))) {
  # operationalised as the mean intensity of all included AUs [! ADJUST SCRIPT !]
  df.exp = rbind(
    df %>%
      filter(task == "H") %>%
      select(ID, dyad, task, speaker, frame, AU01_r, AU02_r, AU06_r, AU07_r,
             AU09_r, AU15_r, AU17_r, AU20_r, AU23_r, AU25_r, AU26_r, AU45_r) %>%
      pivot_longer(names_to = "input", values_to = "exp", cols = matches("AU.*r")) %>%
      group_by(ID, dyad, task, speaker, frame) %>%
      summarise(
        exp = mean(exp, na.rm = T)
      ),
    df %>%
      filter(task == "M") %>%
      select(ID, dyad, task, speaker, frame, AU01_r, AU02_r, AU06_r, AU07_r,
             AU09_r, AU14_r, AU15_r, AU17_r, AU20_r, AU25_r, AU26_r, AU45_r) %>%
      pivot_longer(names_to = "input", values_to = "exp", cols = matches("AU.*r")) %>%
      group_by(ID, dyad, task, speaker, frame) %>%
      summarise(
        exp  = mean(exp, na.rm = T)
      )
  )
  
  # compute synchrony of facial expressiveness by looping through dyads
  df.exp.sync = data.frame()
  sampRate = 30
  for (i in unique(df.exp$dyad)){ 
    
    # initialise heatmaps
    dir.create(file.path(dt.path[1], 'pics'), showWarnings = FALSE)
    pdf(file.path(dt.path[1], 'pics', paste0(i, "_FEsync.pdf")))

    ## HOBBIES
    
    # grab only relevant portions of df
    df.sel = df.exp %>%
      filter(dyad == i & task == "H") %>%
      arrange(ID, dyad, task, speaker, frame)
    
    # check if data frame is present
    if (nrow(df.sel) > 0) { 
      
      # prepare fake MEA components
      s1 = df.sel[df.sel$speaker == "L",]$exp # exp for left L participant
      s2 = df.sel[df.sel$speaker == "R",]$exp # exp for right R participant
      
      # create fake MEA object
      mea = fakeMEA(s1, s2, sampRate) 
      
      # time lagged windowed cross-correlations
      mea = MEAccf(mea, lagSec = 2, winSec = 7, incSec = 4, r2Z = T, ABS = T) 
      names(mea) = paste(i, "H_exp", sep = "_")
      
      # add object to fakeMEA list
      ls.fakeFE = c(ls.fakeFE, mea)
      
      # extract matrix with all ccf values over all lags and windows 
      df.ccf = mea[[1]][["ccf"]] 
      
      # configure heatmap
      par(col.main='white')                  # set plot title to white
      heatmap = MEAheatmap(mea[[1]])
      par(col.main='black')                  # set plot title back to black
      title(main = paste("exp_H", sep = "_")) # alternative title
      
      # peak picking
      L = apply(df.ccf[,1:floor(ncol(df.ccf)/2)], 1, max, na.rm =T ) 
      R = apply(df.ccf[,(floor(ncol(df.ccf)/2)+2):ncol(df.ccf)], 1, max, na.rm =T )
      df.dyad = as.data.frame(cbind(L,R)) %>%
        rownames_to_column(var = "window") %>%
        mutate(
          dyad = i,
          task = "H"
        ) %>%
        pivot_longer(cols = c("L", "R"), names_to = "speaker", values_to = "exp.sync") %>%
        mutate(
          exp.sync = if_else(exp.sync != -Inf & !is.na(exp.sync), exp.sync, NA)
        )
      
      df.exp.sync = rbind(df.exp.sync, df.dyad)
    }
    
    ## MEALPLANNING
    
    # grab only relevant portions of df
    df.sel = df.exp %>%
      filter(dyad == i & task == "M") %>%
      arrange(ID, dyad, task, speaker, frame)
    
    # check if data frame is present
    if (nrow(df.sel) > 0) { 
      
      # prepare fake MEA components
      s1 = df.sel[df.sel$speaker == "L",]$exp # exp for left CTR participant
      s2 = df.sel[df.sel$speaker == "R",]$exp # exp for right BPD participant
      
      # create fake MEA object
      mea = fakeMEA(s1, s2, sampRate) 
      
      # time lagged windowed cross-correlations
      mea = MEAccf(mea, lagSec = 2, winSec = 7, incSec = 4, r2Z = T, ABS = T) 
      names(mea) = paste(i, "M_exp", sep = "_")
      
      # add object to fakeMEA list
      ls.fakeFE = c(ls.fakeFE, mea)
      
      # extract matrix with all ccf values over all lags and windows 
      df.ccf = mea[[1]][["ccf"]] 
      
      # configure heatmap
      par(col.main='white')                  # set plot title to white
      heatmap = MEAheatmap(mea[[1]])
      par(col.main='black')                  # set plot title back to black
      title(main = paste("exp_M", sep = "_")) # alternative title
      
      # peak picking
      L = apply(df.ccf[,1:floor(ncol(df.ccf)/2)], 1, max, na.rm =T ) 
      R = apply(df.ccf[,(floor(ncol(df.ccf)/2)+2):ncol(df.ccf)], 1, max, na.rm =T )
      df.dyad = as.data.frame(cbind(L,R)) %>%
        rownames_to_column(var = "window") %>%
        mutate(
          dyad = i,
          task = "M"
        ) %>%
        pivot_longer(cols = c("L", "R"), names_to = "speaker", values_to = "exp.sync") %>%
        mutate(
          exp.sync = if_else(exp.sync != -Inf & !is.na(exp.sync), exp.sync, NA)
        )
      
      df.exp.sync = rbind(df.exp.sync, df.dyad)
    }
    
    dev.off()
    # show progress
    print(paste(i, "done"))
  }
  
  save(df.exp, df.exp.sync, ls.fakeFE, file = file.path(dt.path[1], "BOKI_exp.RData"))
  
} else {
  
  load(file.path(dt.path[1], "BOKI_exp.RData"))
  
}

# calculate summary statistics
df.exp.sync_NM = df.exp.sync %>%
  group_by(dyad, speaker, task) %>%
  summarise(
    exp.sync_min  = min(exp.sync, na.rm = T),
    exp.sync_max  = max(exp.sync, na.rm = T),
    exp.sync_sd   = sd(exp.sync, na.rm = T),
    exp.sync_mean = mean(exp.sync, na.rm = T),
    exp.sync_md   = median(exp.sync, na.rm = T),
    exp.sync_kurt = kurtosis(exp.sync, na.rm = T),
    exp.sync_skew = skewness(exp.sync, na.rm = T)
  ) %>%
  mutate(
    ID = paste0(dyad, "_", speaker)
  ) %>%
  pivot_wider(names_from = "task", values_from = where(is.numeric)) %>%
  merge(., df %>% select(ID, label, diagnosis) %>% distinct()) %>%
  relocate(ID, dyad, speaker, label)

# full expressiveness: mean of all AUs and frames
df.exp_NM = df %>%
  select(ID, dyad, task, speaker, frame, matches("AU.*r")) %>%
  pivot_longer(names_to = "input", values_to = "exp", cols = matches("AU.*r")) %>%
  group_by(ID, dyad, task, speaker) %>%
  summarise(
    exp = mean(exp, na.rm = T)
  ) %>%
  pivot_wider(names_from = task, values_from = exp, 
              names_glue = "mean_intensity_{task}") %>%
  merge(., df %>% select(ID, label) %>% distinct()) %>%
  relocate(ID, dyad, speaker, label)

# save
write_csv(df.exp_NM, file.path(dt.path[2], "BOKI_FE_intensity_NM.csv"))

# clean workspace
rm(list = setdiff(ls(), c("df", "df.AU.sync", "df.AU.sync_NM", "df.exp", 
                          "df.exp.sync", "df.exp_NM", "ls.fakeAU", "ls.fakeFE",
                          "ls.AUs.M", "ls.AUs.H", "dt.path")))

# Save workspace ----------------------------------------------------------

# save workspace
save.image(file = file.path(dt.path[1], "OpenFace.RData"))

