# (C) Irene Sophia Plank (10planki@gmail.com)

# This script takes all the scripts produced in this pipeline and merges them to
# two csv files, one containing all values of the individual speakers and one
# containing all the values of the dyad. Additionally, it allows for the 
# exclusion of speakers' turn-based information if the categorisation into 
# silences and sounding instances by the uhm-o-meter was suboptimal. The script
# also computes the silence-to-turn ratio of the dyad based on the phonation
# time and the duration of the silence. It also summarises the turn-taking gaps
# by taking the median of all turn-taking gaps for the speaker and the dyad. 

# Set working directory and load libraries --------------------------------

setwd("/home/emba/Documents/ML_BOKI/Data_speech/")

library(tidyverse)

# Load data ---------------------------------------------------------------

# list of participants with low quality of silence information
exc.indi = readRDS("excindi.RDS")

# list of dyads that include one of those participants
exc.dyad = readRDS("excdyad.RDS")

# synchronisation data - information per dyad
df.dyad = read_csv("ML_sync-dyad.csv")

# set dyads with low quality silence detection to NA
for (e in 1:length(exc.dyad)) {
  df.dyad[df.dyad$name == exc.dyad[e], c("pit_sync_MEA", "int_sync_MEA")] = NA
}

df.dyad[c("dyad1", "dyad2", "task")] = str_split_fixed(df.dyad$name, "_", 3)
df.dyad$dyad = paste(df.dyad$dyad1, df.dyad$dyad2, sep = "_")
df.dyad = df.dyad %>% select(-c(name, dyad1, dyad2))

# Pitch intensity and prosodic information --------------------------------

# pitch-intensity and prosodic - information per individual
df.pint = read_csv("ML_pitch_intensity.csv")
df.pros = read_csv("ML_prosodic_extraction.csv")
colnames(df.pros) = c("soundname", "nsyll", "npause", "dur", "pho", "spr",
                      "art", "asd")

# set participants with low quality silence detection to NA
for (e in 1:length(exc.indi)) {
  df.pros[df.pros$soundname == exc.indi[e], c("nsyll", "npause", "pho", "spr",
                                          "art", "asd")] = NA
}

# merge pitch-intensity and prosodic information
df.indi = merge(df.pint,df.pros)
rm(df.pint, df.pros)

# split filenames
df.indi[c("1", "speaker", "dyad1", "dyad2", "left", "right", "task")] = 
  str_split_fixed(df.indi$soundname, "_", 7)
df.indi$dyad = paste(df.indi$dyad1, df.indi$dyad2, sep = "_")
df.indi$task = gsub("[[:digit:]]", "", df.indi$task)

# calculate speech rate for dyad
df.spr = df.indi %>% 
  group_by(dyad, task) %>% 
  summarise(
    nsyll = sum(nsyll, na.rm = T), 
    dur   = mean(dur)
  ) %>%
  distinct() %>%
  mutate(
    spr = nsyll / dur
  ) %>%
  select(dyad, task, spr)

# merge speech rate with dyad data frame
df.dyad = merge(df.dyad, df.spr)
rm(df.spr)

# get rid of unnecessary columns
df.indi = df.indi %>% select(-"1", -dyad1, -dyad2, -soundname)

# Individual synchronisation ----------------------------------------------

# load data
df.sync = read_csv("ML_sync-indi.csv")

# set dyads with one participant with low quality silence detection to NA
for (e in 1:length(exc.dyad)) {
  df.sync[df.sync$name == exc.dyad[e], c("pit_sync", "int_sync")] = NA
}

# adjust individual synchronisation data frame
df.sync[c("dyad1", "dyad2", "task")] = str_split_fixed(df.sync$name, "_", 3)
df.sync$dyad = paste(df.sync$dyad1, df.sync$dyad2, sep = "_")
df.sync = df.sync %>% select(-c(name, dyad1, dyad2))

# sync data frames
df.indi = merge(df.indi,df.sync, all.x = T)
rm(df.sync)

# Silence-to-turn ratio ---------------------------------------------------

str.dyad = df.indi %>%
  group_by(dyad, task) %>%
  summarise(
    dur = mean(dur),
    pho = sum(pho)
  ) %>% 
  mutate(
    sil  = dur - pho) %>%  # silence time
  mutate(
    str  = sil/pho         # pause to turn ratio
  )

df.dyad = merge(df.dyad, str.dyad)
rm(str.dyad)

# Turn-taking gaps --------------------------------------------------------

# load data
df.turn = read_csv("ML_turns.csv")
df.turn$name = paste(df.turn$dyad, df.turn$task, sep = "_")

# set dyads with one participant with low quality silence detection to NA
for (e in 1:length(exc.dyad)) {
  df.turn[df.turn$name == exc.dyad[e], c("ttg")] = NA
}

# summarise turn-taking gaps and number of turns
ttg.indi = df.turn %>%
  group_by(dyad, task, speaker) %>%
  summarise(
    ttg = median(ttg, na.rm = T)
  )
df.indi = merge(df.indi, ttg.indi, all.x = T)

ttg.dyad = df.turn %>%
  group_by(dyad, task) %>%
  summarise(
    ttg      = median(ttg, na.rm = T),
    no_turns = n()
  )
df.dyad = merge(df.dyad, ttg.dyad, all.x = T)

rm(ttg.indi, ttg.dyad, df.turn)

# Add groups and info -----------------------------------------------------

df.indi = df.indi %>% 
  mutate(
    pit_var = sd_pitch^2, 
    int_var = sd_int^2,
    subject = paste(df.indi$dyad, df.indi$speaker, sep = "_"),
    `dyad type` = case_when(
      left == "ASD" | right == "ASD" ~ "heterogeneous",
      left == "TD"  & right == "TD"  ~ "homogeneous"
    ),
    `diagnostic status` = case_when(
      speaker == "L" ~ left,
      speaker == "R" ~ right
    )
    ) %>%
  select(-left, -right, -speaker) %>%
  relocate(subject, dyad, `dyad type`, `diagnostic status`, task)

# add dyad group to dyad data frame
df.sel  = df.indi[,c("dyad","task","dyad type")] %>% distinct()
df.dyad = merge(df.dyad, df.sel) %>%
  relocate(dyad, `dyad type`, task)
rm(df.sel)

# save it all
write_csv(df.dyad, 'ML_dyad.csv')
write_csv(df.indi, 'ML_indi.csv')

# Convert to JASP ---------------------------------------------------------

df.indi_JASP = df.indi %>%
  pivot_wider(names_from = task, values_from = names(df.indi)[(which(names(df.indi) == "task")+1):ncol(df.indi)]) %>%
  mutate( # here we add averages over task in case we need non-parametric tests
    pit_var  = (pit_var_hobbies + pit_var_mealplanning)/2,
    int_var  = (int_var_hobbies + int_var_mealplanning)/2,
    art      = (art_hobbies + art_mealplanning)/2,
    pit_sync = (pit_sync_hobbies + pit_sync_mealplanning)/2,
    int_sync = (pit_sync_hobbies + pit_sync_mealplanning)/2,
    art_sync = (art_sync_hobbies + art_sync_mealplanning)/2
  )

df.dyad_JASP = df.dyad %>%
  pivot_wider(names_from = task, values_from = names(df.dyad)[(which(names(df.dyad) == "task")+1):ncol(df.dyad)]) %>%
  mutate( # here we add averages over task in case we need non-parametric tests
    ttg          = (ttg_hobbies + ttg_mealplanning)/2,
    str          = (str_hobbies + str_mealplanning)/2,
    pit_sync_MEA = (pit_sync_MEA_hobbies + pit_sync_MEA_mealplanning)/2,
    int_sync_MEA = (pit_sync_MEA_hobbies + pit_sync_MEA_mealplanning)/2
  )

# Check variance homogeneity ----------------------------------------------

# Outcomes on the level of the speaker
ls.v = c("pit_var", "int_var", "art", "pit_sync", "int_sync", "art_sync")
task = c("hobbies", "mealplanning")
print('Assumption violated for:')
for (v in ls.v) {
  for (t in task) {
    res = var.test(formula(paste(paste(v, t, sep = "_"), "~ `diagnostic status`")), data = df.indi_JASP)
    if (res$p.value < 0.05) {
      print(paste(v, t, sep = "_"))
    }
  }
}
# [1] "pit_var_hobbies"
# [1] "pit_var_mealplanning"

# Outcomes on the level of the dyad
ls.v = c("ttg", "str", "pit_sync_MEA", "int_sync_MEA")
task = c("hobbies", "mealplanning")
print('Assumption violated for:')
for (v in ls.v) {
  for (t in task) {
    res = var.test(formula(paste(paste(v, t, sep = "_"), "~ `dyad type`")), data = df.dyad_JASP)
    if (res$p.value < 0.05) {
      print(paste(v, t, sep = "_"))
    }
  }
}
# None.

# Save merged csv files ---------------------------------------------------

write_csv(df.dyad_JASP, 'ML_dyad_JASP.csv')
write_csv(df.indi_JASP, 'ML_indi_JASP.csv')
