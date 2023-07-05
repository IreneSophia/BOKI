# (C) Irene Sophia Plank
# 
# This script takes the output of Motion Energy analysis and calculates inter-
# personal synchrony. It is an adaptation of a script written by Jana Koehler, 
# published in https://github.com/jckoe/MLASS-study. 

# BPD participants always sit on the right, CTR on the left. In MEA, the order
# of the ROI is: CTR_head, CTR_body, BPD_head, BPD_body

# clean workspace
rm(list = ls())

# load libraries
library(tidyverse)
library(rMEA)
library(data.table)    # setDT
library(moments)       # kurtosis, skewness

# set path to MEA files
dt.path = "/home/emba/Documents/ML_BOKI/Data_MEA"


# Read in data ------------------------------------------------------------

# hobbies head
mea.H_head = readMEA(dt.path,
                     sampRate = 30, # frame rate of videos
                     skip=300, # skips the first 10s
                     s1Col = c(1), s2Col = c(3), # set columns according to original MEA ROI assignment
                     s1Name = "CTR", s2Name = "BPD", # BPD participants always sat on the right
                     header = FALSE,
                     namefilt = "H",
                     idOrder = c("x","id","session"),
                     idSep = "_",
                     sep = "")
mea.H_head = setGroup(mea.H_head, "head")

# hobbies body
mea.H_body = readMEA(dt.path,
                     sampRate = 30, # frame rate of videos
                     skip=300, # skips the first 10s
                     s1Col = c(2), s2Col = c(4), # set columns according to original MEA ROI assignment
                     s1Name = "CTR", s2Name = "BPD", # BPD participants always sat on the right
                     header = FALSE,
                     namefilt = "H",
                     idOrder = c("x","id","session"),
                     idSep = "_",
                     sep = "")
mea.H_body = setGroup(mea.H_body, "body")


# mealplanning head
mea.M_head = readMEA(dt.path,
                     sampRate = 30, # frame rate of videos
                     skip=300, # skips the first 10s
                     s1Col = c(1), s2Col = c(3), # set columns according to original MEA ROI assignment
                     s1Name = "CTR", s2Name = "BPD", # BPD participants always sat on the right
                     header = FALSE,
                     namefilt = "M",
                     idOrder = c("x","id","session"),
                     idSep = "_",
                     sep = "")
mea.M_head = setGroup(mea.M_head, "head")

# mealplanning body
mea.M_body = readMEA(dt.path,
                     sampRate = 30, # frame rate of videos
                     skip=300, # skips the first 10s
                     s1Col = c(2), s2Col = c(4), # set columns according to original MEA ROI assignment
                     s1Name = "CTR", s2Name = "BPD", # BPD participants always sat on the right
                     header = FALSE,
                     namefilt = "M",
                     idOrder = c("x","id","session"),
                     idSep = "_",
                     sep = "")
mea.M_body = setGroup(mea.M_body, "body")

# combine all together
mea = c(mea.H_body, mea.H_head, mea.M_body, mea.M_head)

# Preprocessing -----------------------------------------------------------

# visual inspection
pdf(file = paste(dt.path, "raw_histograms.pdf", sep = "/"))  
for (i in 1:length(mea)){
  plot(mea[[i]], from=0, to=600, rescale = FALSE) 
}
dev.off()

# scaling
mea.scaled = MEAscale(mea)

# Time series synchronisation ---------------------------------------------

# compute windowed lagged cross correlation
mea.ccf = MEAccf(mea.scaled,
                 lagSec = 5,
                 winSec = 30, 
                 incSec = 15, 
                 r2Z = T,
                 ABS = T)

# visual inspection
pdf(file = paste(dt.path, "heatmaps_IPS.pdf", sep = "/"))  
for (i in 1:length(mea.ccf)){
  MEAheatmap(mea.ccf[[i]], legendSteps = 20, rescale = T) 
}
dev.off()

# convert from mea list to list
ls.ccf = getCCF(mea.ccf, type = "fullMatrix")

# peak picking
for (i in 1:length(ls.ccf)){
  # append maximum of positive lag (s1 movement happening before s2 movement)
  ls.ccf[[i]]$BPD = apply(ls.ccf[[i]][,152:301], 1, max, na.rm = T)
  # append maximum of negative lag (s2 movement happening before s1 movement)
  ls.ccf[[i]]$CTR = apply(ls.ccf[[i]][,1:150], 1, max, na.rm = T) 
  # keep only relevant columns
  ls.ccf[[i]] = ls.ccf[[i]][,c("CTR","BPD")]
  # transpose all df in list
  ls.ccf[[i]] = as.data.frame(t(ls.ccf[[i]]))
  # set rownames as first column
  setDT(ls.ccf[[i]], keep.rownames = TRUE)
  colnames(ls.ccf[[i]])[1] = "position"
}

# create one overall dataframe in the format ID-peaks
df.ccf = bind_rows(ls.ccf, .id = "ID") %>% 
  separate(ID, c("ROI", "dyad", "task")) %>%
  rename("speaker" = "position") %>%
  pivot_longer(cols = starts_with("w"), names_to = "window", values_to = "sync") %>%
  mutate(
    sync = if_else(sync != -Inf, sync, NA)
  ) %>% 
  group_by(dyad, speaker, ROI, task) %>%
  summarise(
    sync.min  = min(sync, na.rm = T),
    sync.max  = max(sync, na.rm = T),
    sync.sd   = sd(sync, na.rm = T),
    sync.mean = mean(sync, na.rm = T),
    sync.md   = median(sync, na.rm = T),
    sync.kurt = kurtosis(sync, na.rm = T),
    sync.skew = skewness(sync, na.rm = T)
  )

# create and save NM data frame for head
df.MEAhead_NM = df.ccf %>%
  mutate(
    ID = paste0(dyad, "_", speaker)
  ) %>% relocate(ID) %>%
  filter(ROI == "head") %>%
  pivot_wider(names_from = task, values_from = matches("sync.*"))
write.csv(df.MEAhead_NM, "mea_ccf_head.csv")

# create and save NM data frame for body
df.MEAbody_NM = df.ccf %>%
  mutate(
    ID = paste0(dyad, "_", speaker)
  ) %>% relocate(ID) %>%
  filter(ROI == "body") %>%
  pivot_wider(names_from = task, values_from = matches("sync.*"))
write.csv(df.MEAbody_NM, "mea_ccf_body.csv")

# Movement quantity -------------------------------------------------------

# extract information from mea object
df.mov = summary(mea.ccf)[, c("CTR_%", "BPD_%")] %>%
  rownames_to_column(var = "ID") %>%
  separate(ID, c("ROI", "dyad", "task")) %>%
  rename("BPD" = "BPD_%", "CTR" = "CTR_%") %>%
  pivot_longer(cols = c("BPD", "CTR"), names_to = "speaker", values_to = "movement")

# create and save NM data frame for movement
df.MEAmov_NM = df.mov %>%
  mutate(
    ID = paste0("BOKI_", dyad, "_", speaker)
  ) %>% relocate(ID) %>%
  pivot_wider(names_from = c(task, ROI), values_from = movement)
write.csv(df.MEAmovb_NM, "movementquantity.csv")
