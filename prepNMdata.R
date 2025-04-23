# SCRIPT FOR PREPARATION OF IDs, LABELS, FEATURES, AND COVARIATES FOR NEUROMINER

# Clear global environment
rm(list=ls())

# load libraries
library("data.table")
library("stringr")
library("tidyverse")

# set file path
dt.path = "/media/emba/emba-2/ML_BOKI/ML_data"

# 1. READ IN DATA ---------------------------------------------------------

ls.df = list(
  read_csv(file.path(dt.path, "BOKI_FE_intensity_NM.csv"), show_col_types = F),
  read_csv(file.path(dt.path, "BOKI_FE_syncentrain_NM.csv"), show_col_types = F),
  read_csv(file.path(dt.path, "BOKI_mea_NM.csv"), show_col_types = F),
  read_csv(file.path(dt.path, "BOKI_movementquantity_NM.csv"), show_col_types = F),
  read_csv(file.path(dt.path, "BOKI_peaks_intra_NM.csv"), show_col_types = F),
  read_csv(file.path(dt.path, "BOKI_speech_NM.csv"), show_col_types = F)
)

df = ls.df %>% reduce(full_join, by = c("ID", "dyad", "speaker", "label")) %>%
  select(-speaker)

# BERT
df.bert = list.files(path = "/media/emba/emba-2/ML_BOKI/BERT_raw", 
                     pattern = "*.csv", full.names = F) %>%
  setNames(nm = .) %>%
  map_df(~read_csv(file.path("/media/emba/emba-2/ML_BOKI/BERT_raw", .), 
                   show_col_types = F), .id = "fln") %>%
  mutate(
    ID = sprintf("BOKI_%02d_%s", 
                 as.numeric(gsub("\\D", "", fln)),
                 gsub(".*_(.+).csv", "\\1", fln))
  ) %>%
  group_by(ID) %>%
  summarise(
    acc = mean(correct),
    rt  = mean(response_time, na.rm = T)
  )

# merge together
df = merge(df, df.bert, all.x = T)

# 2. MERGE WITH DATA FROM KOEHLER 2024 & PLANK 2023  ----------------------

df.koehler = read_csv(file.path(dt.path, "TIDY_NM_inputdata.csv")) %>%
  select(-`...1`, -label, -indclasslabel) %>%
  mutate(
    dyad  = substr(ID, 1,6),
    label = case_match(dyadclasslabel,
                       "ASD-TD" ~ "ASD-COMP",
                       "TD-TD"  ~ "COMP-COMP"),
    ID = paste0(dyad, substr(ID, nchar(ID)-1, nchar(ID)))
  ) %>%
  # rename the tasks to H and M consistently
  rename_with(~ gsub("hobbies", "H", .x)) %>%
  rename_with(~ gsub("mealplanning", "M", .x)) %>%
  rename_with(~ gsub("_h$", "_H", .x)) %>%
  rename_with(~ gsub("_mp$", "_M", .x)) %>%
  select(-dyadclasslabel) %>%
  relocate(ID, dyad, label)

df.plank = read_csv(file.path(dt.path, "ML_indi_context.csv")) %>%
  rename("ID" = "Id") %>%
  rename_with(~ gsub("hobbies", "H_speech", .x)) %>%
  rename_with(~ gsub("mealplanning", "M_speech", .x)) %>%
  mutate(
    label = case_match(`dyad type`,
                                "heterogeneous" ~ "ASD-COMP", 
                                "homogeneous"   ~ "COMP-COMP"),
    dyad  = substr(ID, 1,6)
  ) %>%
  select(-spr_H_speech, -spr_M_speech, -`dyad type`) %>%
  relocate(ID, dyad, label)

# 4 dyads removed due to bad OF quality > should/could we replace them?

df.prev = merge(df.koehler, df.plank)

df.all = rbind(df.prev, df) %>%
  mutate(
    labelNo = as.numeric(as.factor(label))
  ) %>% 
  relocate(
    ID, dyad, label, labelNo
  ) %>%
  arrange(
    label, ID
  )


# 3. EXPORT ---------------------------------------------------------------

write_csv(df.all, file.path(dt.path, "BOKI_NM_inputdata.csv"))


# 4. CORRELATION PLOTS OF FEATURES ----------------------------------------

FE_sync_complete    = df.all[,grepl("AU",colnames(df.all))]
head_sync_complete  = df.all[,grepl("headsync",colnames(df.all)) | grepl("pose_R",colnames(df.all))]
body_sync_complete  = df.all[,grepl("bodysync",colnames(df.all))]
intra_sync_complete = df.all[,grepl("bodysync",colnames(df.all))]
total_sync_complete = df.all[,grepl("total_movement",colnames(df.all)) | grepl("intensity",colnames(df.all))]
speech_complete     = df.all[,grepl("speech",colnames(df.all))]

heatmap(cor(FE_sync_complete),    Colv = NA, Rowv = NA)
heatmap(cor(head_sync_complete),  Colv = NA, Rowv = NA)
heatmap(cor(body_sync_complete),  Colv = NA, Rowv = NA)
heatmap(cor(intra_sync_complete), Colv = NA, Rowv = NA)
heatmap(cor(total_sync_complete), Colv = NA, Rowv = NA)
heatmap(cor(speech_complete),     Colv = NA, Rowv = NA)