
# Packages ----------------------------------------------------------------

# if packman is not installed yet, install it
if(!("pacman" %in% installed.packages()[,"Package"])) install.packages("pacman")
pacman::p_load(tidyverse, rMEA, moments) #, signal, foreach, doParallel

# set frames per second for videos
fps = 30

# set path to files
dt.pathX = c("/media/emba/emba-2/ML_BOKI/OpenFace_preprocessed", 
            "/media/emba/emba-2/ML_BOKI/AUD_preprocessed", 
            "/media/emba/emba-2/ML_BOKI/MEA_preprocessed", 
            "/media/emba/emba-2/ML_BOKI/MLASS_txtfiles", 
            "/media/emba/emba-2/ML_BOKI/ML_data")

ls.dyads = c("BOKI_08", "BOKI_10", "BOKI_13", "BOKI_14", "BOKI_15", "BOKI_16", 
             "BOKI_17", "BOKI_19", "BOKI_24", "BOKI_25", "BOKI_26", "BOKI_27", 
             "BOKI_28", "BOKI_29", "BOKI_34", "BOKI_36", "BOKI_37", "BOKI_40", 
             "BOKI_42", "BOKI_43", "BOKI_49", "BOKI_50", "BOKI_51", "BOKI_53", 
             "BOKI_54", "BOKI_57", "BOKI_58", "BOKI_59", "BOKI_61", 
             "ML_001", "ML_002", "ML_003", "ML_004", "ML_005", "ML_006", 
             "ML_008", "ML_009", "ML_010", "ML_014", "ML_016", "ML_017", 
             "ML_019", "ML_023", "ML_025", "ML_026", "ML_027", "ML_030", 
             "ML_031", "ML_032", "ML_033", "ML_034", "ML_036", "ML_037", 
             "ML_039", "ML_040", "ML_042", "ML_044", "ML_045", "ML_049", 
             "ML_050")

# Read in Data ------------------------------------------------------------
  
# MEA DATA

# initialise dataframe
df.mea = data.frame()

# BOKI stuff
load(file.path(dt.pathX[3], 'MEA.Rdata'))

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
mea.H_head = readMEA(dt.pathX[4],
                     sampRate = 30, # frame rate of videos
                     skip = 300, # skips the first 10s
                     nrows = 18000,
                     s1Col = c(1), s2Col = c(3), # set columns according to original MEA ROI assignment
                     s1Name = "L", s2Name = "R", # BPD participants always sat on the right
                     header = FALSE,
                     namefilt = "hobbies",
                     idOrder = c("x","id"),
                     idSep = "_",
                     sep = "")
mea.H_head = setGroup(mea.H_head, "head_H")
mea.H_body = readMEA(dt.pathX[4],
                     sampRate = 30, # frame rate of videos
                     skip = 300, # skips the first 10s
                     nrows = 18000,
                     s1Col = c(2), s2Col = c(4), # set columns according to original MEA ROI assignment
                     s1Name = "L", s2Name = "R", # BPD participants always sat on the right
                     header = FALSE,
                     namefilt = "hobbies",
                     idOrder = c("x","id"),
                     idSep = "_",
                     sep = "")
mea.H_body = setGroup(mea.H_body, "body_H")
mea.M_head = readMEA(dt.pathX[4],
                     sampRate = 30, # frame rate of videos
                     skip = 300, # skips the first 10s
                     nrows = 18000,
                     s1Col = c(1), s2Col = c(3), # set columns according to original MEA ROI assignment
                     s1Name = "L", s2Name = "R", # BPD participants always sat on the right
                     header = FALSE,
                     namefilt = "mealplanning",
                     idOrder = c("x","id"),
                     idSep = "_",
                     sep = "")
mea.M_head = setGroup(mea.M_head, "head_M")
mea.M_body = readMEA(dt.pathX[4],
                     sampRate = 30, # frame rate of videos
                     skip = 300, # skips the first 10s
                     nrows = 18000,
                     s1Col = c(2), s2Col = c(4), # set columns according to original MEA ROI assignment
                     s1Name = "L", s2Name = "R", # BPD participants always sat on the right
                     header = FALSE,
                     namefilt = "mealplanning",
                     idOrder = c("x","id"),
                     idSep = "_",
                     sep = "")
mea.M_body = setGroup(mea.M_body, "body_M")

# combine all together
mea = c(mea.H_body, mea.H_head, mea.M_body, mea.M_head)

# scaling
mea.scaled = MEAscale(mea)

for (i in 1:length(mea.scaled)) {
  L = as.numeric(mea.scaled[[i]]$MEA$L)
  R = as.numeric(mea.scaled[[i]]$MEA$R)
  info = str_split(names(mea.scaled[i]), pattern = "_")
  dyad = rep(sprintf("ML_%s", info[[1]][3]), times = 18000)
  task = rep(info[[1]][2], times = 18000)
  ROI  = rep(info[[1]][1], times = 18000)
  frame = 1:18000
  df.mea = rbind(df.mea, data.frame(dyad, task, ROI, frame, L, R))
}
df.mea = df.mea %>%
  filter(dyad %in% ls.dyads) %>% 
  pivot_longer(cols = c(L, R), names_to = "speaker") %>%
  pivot_wider(names_from = ROI, names_prefix = "MEA_")

# OPEN FACE DATA

df.MLASS = readRDS(file.path(dt.pathX[1], "MLASS_OF.rds")) %>%
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
  ) %>% select(-timestamp)

df.OF = readRDS(file.path(dt.pathX[1], "BOKI_OF.rds")) %>% 
  select(-c(face_id, pose_Tx, pose_Ty, pose_Tz)) %>% 
  select(-timestamp) %>%
  rbind(., df.MLASS) %>%
  mutate(
    dyad = if_else(substr(dyad,1,1) == "0", paste0("ML_", dyad), dyad), 
    # adjust the frame number, because MEA starts with 1 and OF with 301
    frame = frame - 300
  ) %>%
  filter(dyad %in% ls.dyads)

# MERGE VIDEO DATA
df.vid = merge(df.OF, df.mea) %>%
  mutate(
    turn_self  = 0,
    turn_other = 0
  ) %>%
  relocate(dyad, label, ID, task, speaker, diagnosis, frame) %>%
  arrange(ID, task, frame)

# SPEECH DATA

# read in the turn information
df.turns = rbind(read_csv(file.path(dt.pathX[2], "OUT_turns.csv")) %>% select(-turn),
                 read_csv(file.path(dt.pathX[2], "ML_turns.csv")) %>%
                   mutate(
                     # in the previous paper, the first 10s were removed later
                     start_turn = start_turn - 10000,
                     end_turn = end_turn - 10000,
                     # make sure the task is labelled the same
                     task = if_else(task == "mealplanning", "M", "H")
                   )) %>%
  mutate_if(is.character, as.factor) %>%
  filter(dyad %in% ls.dyads)

# clean workspace
rm(list = setdiff(ls(), c("df.vid", "df.turns", "dt.pathX", "ls.dyads", "fps")))

# Add turns to vid data ---------------------------------------------------

# calculate start and end frame out of the timing
df.turns = df.turns %>%
  mutate(
    # calculate the video frames from the time > ceiling since starts with 1
    start_frame = ceiling(start_turn / (1000/fps)),
    end_frame   = ceiling(end_turn / (1000/fps))
  ) %>%
  select(dyad, task, speaker, start_frame, end_frame) %>%
  rename("speaker_turn" = "speaker") %>%
  pivot_longer(cols = c(start_frame, end_frame), 
               names_to = "trigger", values_to = "frame")

# merge with video data frame
df = merge(df.turns, 
           df.vid %>% mutate(vid_row = row_number()), 
           all = T) %>%
  # get rid of overlapping frames
  arrange(vid_row) %>% 
  mutate(
    row_diff = vid_row - lag(vid_row)
  ) %>%
  filter(row_diff == 1 | is.na(row_diff)) %>%
  mutate(
    # create a column for their own turn
    turn_self = case_when(
      speaker == speaker_turn & trigger == "start_frame" ~ 1,
      speaker == speaker_turn & trigger == "end_frame" ~ 0,
      T ~ NA
    ),
    # create a column for their partners turn
    turn_other = case_when(
      speaker != speaker_turn & trigger == "start_frame" ~ 1,
      speaker != speaker_turn & trigger == "end_frame" ~ 0,
      T ~ NA
    )
  ) %>%
  # fill up the turn information for all frames in between
  fill(c(turn_self, turn_other), .direction = "down") %>%
  mutate(
    turn_self  = replace_na(turn_self, 0),
    turn_other = replace_na(turn_other, 0)
  ) %>%
  select(-speaker_turn, -trigger, -row_diff, -vid_row) %>%
  mutate_if(is.character, as.factor) %>%
  relocate(dyad, label, ID, speaker, diagnosis, task, frame) %>%
  arrange(ID, task, frame)

# save full data frame
saveRDS(df, file = file.path(dt.pathX[length(dt.pathX)], "OUT_cross.rds"))

# summarise all the information
df.long = df %>%
  select(-c(frame, confidence, success, pose_Rx, pose_Ry, pose_Rz)) %>%
  pivot_longer(cols = -c(dyad, label, ID, speaker, diagnosis, task, turn_self, turn_other)) %>%
  filter(substring(name, nchar(name), nchar(name)) != "c")

# aggregate everything, first for when you are speaking yourself
df.self = df.long %>%
  filter(turn_self == 1) %>%
  select(-c(turn_self, turn_other, ID)) %>%
  group_by(dyad, label, speaker, diagnosis, task, name) %>%
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
  ungroup() %>%
  pivot_wider(names_from = c(task, name), values_from = where(is.numeric),
              names_glue = "self_{.value}_{task}_{name}")

# now, when your partner is speaking
df.other = df.long %>%
  filter(turn_self == 1) %>%
  select(-c(turn_self, turn_other, ID)) %>%
  group_by(dyad, label, speaker, diagnosis, task, name) %>%
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
  ungroup() %>%
  pivot_wider(names_from = c(task, name), values_from = where(is.numeric),
              names_glue = "other_{.value}_{task}_{name}")

# merge both together
df.agg = merge(df.self, df.other) %>%
  mutate(
    ID = paste0(dyad, "_", speaker)
  ) %>%
  relocate(ID, dyad, speaker, label) %>%
  select(-diagnosis)

write_csv(df.agg, file = file.path(dt.pathX[length(dt.pathX)], "BOKI_cross.csv"))
