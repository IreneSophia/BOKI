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
df = merge(df, df.bert, all.x = T) %>%
  mutate(
    ilabel = case_when(
      label == "BPD-COMP" & substr(ID, nchar(ID), nchar(ID)) == "R" ~ "BPD",
      T ~ "COMP"
    )
  ) %>%
  relocate(ID, dyad, label, ilabel)

# 2. MERGE WITH DATA FROM KOEHLER 2024 & PLANK 2023  ----------------------

df.koehler = read_csv(file.path(dt.path, "TIDY_NM_inputdata.csv")) %>%
  select(-`...1`, -label) %>% # HERE INDIVIDUAL
  mutate(
    dyad  = substr(ID, 1,6),
    label = case_match(dyadclasslabel,
                       "ASD-TD" ~ "ASD-COMP",
                       "TD-TD"  ~ "COMP-COMP"),
    ilabel = case_when(
      indclasslabel == "TD" ~ "COMP",
      T ~ indclasslabel
    ),
    ID = paste0(dyad, substr(ID, nchar(ID)-1, nchar(ID)))
  ) %>%
  # rename the tasks to H and M consistently
  rename_with(~ gsub("hobbies", "H", .x)) %>%
  rename_with(~ gsub("mealplanning", "M", .x)) %>%
  rename_with(~ gsub("_h$", "_H", .x)) %>%
  rename_with(~ gsub("_mp$", "_M", .x)) %>%
  select(-dyadclasslabel, -indclasslabel) %>%
  relocate(ID, dyad, label, ilabel)

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

df.all = rbind(df.prev, df)

# 3. ADD NEW CROSS FEATURES -----------------------------------------------

df.new = read_csv(file.path(dt.path, "BOKI_cross.csv"), show_col_types = F) %>%
  select(-speaker)

df.all = merge(df.all, df.new) %>%
  mutate(
    labelNo = as.numeric(as.factor(label))
  )

# 4. ADD DEMOGRAPHICS -----------------------------------------------------

df.all = merge(read_csv(file.path(gsub("ML_data", "demoCentraXX", dt.path), 
                                  "BOKI_centraXX.csv"), show_col_types = F) %>%
                 mutate(
                   ID = gsub("TD_", "", gsub("ASD_", "", ID))
                 ), df.all) %>% 
  relocate(
    ID, dyad, label, labelNo, ilabel
  ) %>%
  arrange(
    label, ID
  )

# 5. EXPORT ---------------------------------------------------------------

write_csv(df.all, file.path(dt.path, "BOKI_NM_inputdata.csv"))
