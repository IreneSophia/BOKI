# (C) Irene Sophia Plank
# 
# This script takes the output of Motion Energy analysis and calculates INTER-
# personal synchrony. It is an adaptation of a script written by Jana Koehler, 
# published in https://github.com/jckoe/MLASS-study. 

# BPD participants always sit on the right, if there is one, COMP on the left. 
# In MEA, the order of the ROI is: L_head, L_body, R_head, R_body

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
            "/media/emba/emba-2/ML_BOKI/ML_data")

# Check dyad data ---------------------------------------------------------

# lists all relevant IDs
df.sub = read_csv(file.path("/media/emba/emba-2/ML_BOKI/demoCentraXX", 
                            "BOKI_centraXX.csv")) %>%
  filter(substr(dyad, 1, 4) == "BOKI") %>%
  select(dyad, ID, label)
ls.inc = unique(df.sub$dyad)
ls.IDs = c(paste0(ls.inc, "_M"), paste0(ls.inc, "_H"))

# check if any files are missing
incomplete = c()
ls.fls = c() # list for all relevant files
for (d in ls.IDs) {
  file = file.path(dt.path[1], paste0(d, '.txt'))
  if (!file.exists(file)) {
    warning(sprintf('File %s does not exist', d))
    incomplete = c(incomplete, d)
  } else {
    ls.fls = c(ls.fls, file)
  }
}

# no incomplete dyads

# Read in data ------------------------------------------------------------

# hobbies head
mea.H_head = readMEA(dt.path[1],
                     sampRate = 30, # frame rate of videos
                     skip = 300, # skips the first 10s
                     nrows = 18000,
                     s1Col = c(1), s2Col = c(3), # set columns according to original MEA ROI assignment
                     s1Name = "L", s2Name = "R", # BPD participants always sat on the right
                     header = FALSE,
                     namefilt = "H",
                     idOrder = c("x","id","session"),
                     idSep = "_",
                     sep = "")
mea.H_head = setGroup(mea.H_head, "head")

# hobbies body
mea.H_body = readMEA(dt.path[1],
                     sampRate = 30, # frame rate of videos
                     skip = 300, # skips the first 10s
                     nrows = 18000,
                     s1Col = c(2), s2Col = c(4), # set columns according to original MEA ROI assignment
                     s1Name = "L", s2Name = "R", # BPD participants always sat on the right
                     header = FALSE,
                     namefilt = "H",
                     idOrder = c("x","id","session"),
                     idSep = "_",
                     sep = "")
mea.H_body = setGroup(mea.H_body, "body")


# mealplanning head
mea.M_head = readMEA(dt.path[1],
                     sampRate = 30, # frame rate of videos
                     skip = 300, # skips the first 10s
                     nrows = 18000,
                     s1Col = c(1), s2Col = c(3), # set columns according to original MEA ROI assignment
                     s1Name = "L", s2Name = "R", # BPD participants always sat on the right
                     header = FALSE,
                     namefilt = "M",
                     idOrder = c("x","id","session"),
                     idSep = "_",
                     sep = "")
mea.M_head = setGroup(mea.M_head, "head")

# mealplanning body
mea.M_body = readMEA(dt.path[1],
                     sampRate = 30, # frame rate of videos
                     skip = 300, # skips the first 10s
                     nrows = 18000,
                     s1Col = c(2), s2Col = c(4), # set columns according to original MEA ROI assignment
                     s1Name = "L", s2Name = "R", # BPD participants always sat on the right
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
pdf(file = paste(dt.path[1], "raw_histograms.pdf", sep = "/"))  
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
pdf(file = paste(dt.path[1], "heatmaps_IPS.pdf", sep = "/"))  
for (i in 1:length(mea.ccf)){
  MEAheatmap(mea.ccf[[i]], legendSteps = 20, rescale = T) 
}
dev.off()

# convert from mea list to list
ls.ccf = getCCF(mea.ccf, type = "fullMatrix")

# peak picking
for (i in 1:length(ls.ccf)){
  # append maximum of positive lag (s1 movement happening before s2 movement)
  ls.ccf[[i]]$R = apply(ls.ccf[[i]][,(floor(ncol(ls.ccf[[i]])/2)+2):ncol(ls.ccf[[i]])], 1, max, na.rm = T)
  # append maximum of negative lag (s2 movement happening before s1 movement)
  ls.ccf[[i]]$L = apply(ls.ccf[[i]][,1:floor(ncol(ls.ccf[[i]])/2)], 1, max, na.rm = T) 
  # keep only relevant columns
  ls.ccf[[i]] = ls.ccf[[i]][,c("L","R")]
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
    MEA.sync = if_else(MEA.sync != -Inf, MEA.sync, NA),
    dyad = paste0("BOKI_", dyad)
  ) %>% 
  group_by(dyad, speaker, ROI, task) %>%
  summarise(
    min  = min(MEA.sync, na.rm = T),
    max  = max(MEA.sync, na.rm = T),
    sd   = sd(MEA.sync, na.rm = T),
    mean = mean(MEA.sync, na.rm = T),
    md   = median(MEA.sync, na.rm = T),
    kurtosis = kurtosis(MEA.sync, na.rm = T),
    skew = skewness(MEA.sync, na.rm = T)
  )

# create and save NM data frame for head
df.MEA_NM = df.ccf %>%
  mutate(
    ID = paste0(dyad, "_", speaker)
  ) %>% 
  pivot_wider(names_from = c(task, ROI), values_from = where(is.numeric),
              names_glue = "{.value}_{task}_{ROI}sync") %>%
  merge(., df.sub %>% select(ID, label) %>% distinct()) %>%
  relocate(ID, dyad, speaker, label)
write_csv(df.MEA_NM, file.path(dt.path[2], "BOKI_mea_NM.csv"))

# Movement quantity -------------------------------------------------------

# extract information from mea object
df.mov = summary(mea.ccf)[, c("L_%", "R_%")] %>%
  rownames_to_column(var = "ID") %>%
  separate(ID, c("ROI", "dyad", "task")) %>%
  rename("R" = "R_%", "L" = "L_%") %>%
  pivot_longer(cols = c("R", "L"), names_to = "speaker", values_to = "MEA.movement")

# create and save NM data frame for movement
df.MEA.mov_NM = df.mov %>%
  mutate(
    ID = paste0("BOKI_", dyad, "_", speaker),
    dyad = paste0("BOKI_", dyad)
  ) %>% relocate(ID) %>%
  pivot_wider(names_from = c(task, ROI), values_from = MEA.movement,
              names_glue = "{task}_{ROI}_total_movement") %>%
  
  merge(., df.sub %>% select(ID, label) %>% distinct()) %>%
  relocate(ID, dyad, speaker, label)
write_csv(df.MEA.mov_NM, file.path(dt.path[2], "BOKI_movementquantity_NM.csv"))

# clean workspace
rm(list = setdiff(ls(), c("df.ccf", "mea.ccf", "df.MEA.mov_NM", "dt.path",
                          "df.MEA.head_NM", "df.MEA.body_NM")))

# Save workspace ----------------------------------------------------------

# save workspace
save.image(file = file.path(dt.path[1], "MEA.Rdata"))

