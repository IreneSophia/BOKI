# (C) Irene Sophia Plank
# 
# This script takes the output of Motion Energy analysis and calculates INTER-
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
library(filesstrings)  # file.move

# set path to MEA files
dt.path = "/home/emba/Documents/ML_BOKI/Data_MEA"
setwd(dt.path)

# Check dyad data ---------------------------------------------------------

# lists all relevant IDs
ls.IDs = list.files(path = dt.path, pattern = "BOKI_.*\\.txt")

# check if there are dyads where one or more files are missing
incomplete = c()
for (d in unique(substr(ls.IDs, 1, 7))) {
  idx = which(substr(ls.IDs, 1, 7) == d)
  if (length(idx) != 2) {
    warning(sprintf('Dyad %s does not have 2 files', d))
    incomplete = c(incomplete, ls.IDs[idx])
  }
}

# move this dyad to another folder
for (i in incomplete) { 
  file.move(paste0(dt.path, "/", i), paste0(dt.path, "/incomplete/"))
}

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
  ls.ccf[[i]]$BPD = apply(ls.ccf[[i]][,(floor(ncol(ls.ccf[[i]])/2)+2):ncol(ls.ccf[[i]])], 1, max, na.rm = T)
  # append maximum of negative lag (s2 movement happening before s1 movement)
  ls.ccf[[i]]$CTR = apply(ls.ccf[[i]][,1:floor(ncol(ls.ccf[[i]])/2)], 1, max, na.rm = T) 
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
  pivot_longer(cols = starts_with("w"), names_to = "window", values_to = "MEA.sync") %>%
  mutate(
    MEA.sync = if_else(MEA.sync != -Inf, MEA.sync, NA)
  ) %>% 
  group_by(dyad, speaker, ROI, task) %>%
  summarise(
    MEA.sync_min  = min(MEA.sync, na.rm = T),
    MEA.sync_max  = max(MEA.sync, na.rm = T),
    MEA.sync_sd   = sd(MEA.sync, na.rm = T),
    MEA.sync_mean = mean(MEA.sync, na.rm = T),
    MEA.sync_md   = median(MEA.sync, na.rm = T),
    MEA.sync_kurt = kurtosis(MEA.sync, na.rm = T),
    MEA.sync_skew = skewness(MEA.sync, na.rm = T)
  )

# create and save NM data frame for head
df.MEA.head_NM = df.ccf %>%
  mutate(
    ID = paste0(dyad, "_", speaker)
  ) %>% relocate(ID) %>%
  filter(ROI == "head") %>%
  pivot_wider(names_from = task, values_from = matches("MEA.sync.*"))
write.csv(df.MEA.head_NM, "mea_ccf_head.csv")

# create and save NM data frame for body
df.MEA.body_NM = df.ccf %>%
  mutate(
    ID = paste0(dyad, "_", speaker)
  ) %>% relocate(ID) %>%
  filter(ROI == "body") %>%
  pivot_wider(names_from = task, values_from = matches("MEA.sync.*"))
write.csv(df.MEA.body_NM, "mea_ccf_body.csv")

# Movement quantity -------------------------------------------------------

# extract information from mea object
df.mov = summary(mea.ccf)[, c("CTR_%", "BPD_%")] %>%
  rownames_to_column(var = "ID") %>%
  separate(ID, c("ROI", "dyad", "task")) %>%
  rename("BPD" = "BPD_%", "CTR" = "CTR_%") %>%
  pivot_longer(cols = c("BPD", "CTR"), names_to = "speaker", values_to = "MEA.movement")

# create and save NM data frame for movement
df.MEA.mov_NM = df.mov %>%
  mutate(
    ID = paste0("BOKI_", dyad, "_", speaker)
  ) %>% relocate(ID) %>%
  pivot_wider(names_from = c(ROI, task), values_from = MEA.movement, names_prefix = "MEA.movement.")
write.csv(df.MEA.mov_NM, "movementquantity.csv")

# clean workspace
rm(list = setdiff(ls(), c("df.ccf", "mea.ccf", "df.MEA.mov_NM", "df.MEA.head_NM", "df.MEA.body_NM")))

# Save workspace ----------------------------------------------------------

# save workspace
save.image(file = "MEA.Rdata")

# save list of fakeMEA objects for pseudosync calculation: contains sampRate, lag, win, inc information
saveRDS(mea.ccf, file = paste0(str_replace(getwd(), "MEA", "psync"), "/interMEA_30_5_30_15.RDS"))
