# Load packages 
library(tidyverse)

# Clear global environment
rm(list=ls())
setwd("/home/emba/Documents/ML_BOKI/CentraXX")

# load raw data
# columns of Interest: internalStudyMemberID, name2, code, value, section, (valueIndex), numericValue
ls.fl = setdiff(list.files(pattern = "BOKI_.*.csv"), "BOKI_centraXX.csv")
df = ls.fl %>%
  map_df(~read_delim(., show_col_types = F, locale = locale(encoding = "ISO-8859-1"))) %>%
  select(internalStudyMemberID, name2, code, value, section, numericValue) %>%
  filter(internalStudyMemberID != "NEVIA_test" & !is.na(name2) & substr(internalStudyMemberID,1,10) != "BOKI_Pilot") %>%
  rename("questionnaire" = "name2", 
         "item" = "code", 
         "SID" = "internalStudyMemberID") %>%
  mutate(
    value = str_replace(value, ",", ";"),
    questionnaire = str_replace(questionnaire, "PSY_BOKI_DEMO_Neu", "PSY_BOKI_DEMO")
  ) %>%
  group_by(SID) %>% distinct()

## preprocess each questionnaire separately:

# PSY_BOKI_BDI 
# (10 bis 19: leichtes depressives Syndrom, 20 bis 29: mittelgradiges, >= 30: schweres)
df.bdi = df %>% filter(questionnaire == "PSY_BOKI_BDI")  %>%
  mutate(
    numericValue = case_when(
      "NEIN" == value ~ "0",
      "Ja" == value ~ "1",
      grepl(", ", numericValue, fixed = T) ~ sub(".*,", "", numericValue),
      !grepl(", ", numericValue, fixed = T) ~ numericValue
      ),
    numericValue = as.numeric(numericValue)) %>%
  select(questionnaire, SID, numericValue) %>%
  group_by(SID) %>%
  summarise(
    BDI_total = sum(numericValue, na.rm = T)
  )

# PSY_BOKI_CFT
df.cft = df %>% filter(questionnaire == "PSY_BOKI_CFT") %>% 
  group_by(SID) %>%
  select(-c(questionnaire, section, numericValue)) %>%
  rename("raw" = `value`) %>%
  mutate(
    item = gsub("^PSY_PIPS_", "", item), 
    score = case_when(
      raw == 'd' & item == 'CFT_1_1' ~ 1, raw == 'b' & item == 'CFT_1_2' ~ 1,raw == 'e' & item == 'CFT_1_3' ~ 1,
      raw == 'a' & item == 'CFT_1_4' ~ 1, raw == 'e' & item == 'CFT_1_5' ~ 1, raw == 'b' & item == 'CFT_1_6' ~ 1, 
      raw == 'c' & item == 'CFT_1_7' ~ 1, raw == 'c' & item == 'CFT_1_8' ~ 1, raw == 'd' & item == 'CFT_1_9' ~ 1, 
      raw == 'a' & item == 'CFT_1_10' ~ 1, raw == 'b' & item == 'CFT_1_11' ~ 1, raw == 'a' & item == 'CFT_1_12' ~ 1, 
      raw == 'c' & item == 'CFT_1_13' ~ 1, raw == 'd' & item == 'CFT_1_14' ~ 1, raw == 'e' & item == 'CFT_1_15' ~ 1, 
      raw == 'd' & item == 'CFT_2_1' ~ 1, raw == 'a' & item == 'CFT_2_2' ~ 1, raw == 'b' & item == 'CFT_2_3' ~ 1, 
      raw == 'a' & item == 'CFT_2_4' ~ 1, raw == 'e' & item == 'CFT_2_5' ~ 1, raw == 'c' & item == 'CFT_2_6' ~ 1, 
      raw == 'b' & item == 'CFT_2_7' ~ 1, raw == 'a' & item == 'CFT_2_8' ~ 1, raw == 'c' & item == 'CFT_2_9' ~ 1, 
      raw == 'e' & item == 'CFT_2_10' ~ 1, raw == 'c' & item == 'CFT_2_11' ~ 1, raw == 'e' & item == 'CFT_2_12' ~ 1, 
      raw == 'd' & item == 'CFT_2_13' ~ 1, raw == 'd' & item == 'CFT_2_14' ~ 1, raw == 'b' & item == 'CFT_2_15' ~ 1, 
      raw == 'b' & item == 'CFT_3_1' ~ 1, raw == 'c' & item == 'CFT_3_2' ~ 1, raw == 'b' & item == 'CFT_3_3' ~ 1, 
      raw == 'd' & item == 'CFT_3_4' ~ 1, raw == 'b' & item == 'CFT_3_5' ~ 1, raw == 'a' & item == 'CFT_3_6' ~ 1, 
      raw == 'e' & item == 'CFT_3_7' ~ 1, raw == 'd' & item == 'CFT_3_8' ~ 1, raw == 'c' & item == 'CFT_3_9' ~ 1, 
      raw == 'a' & item == 'CFT_3_10' ~ 1, raw == 'e' & item == 'CFT_3_11' ~ 1, raw == 'c' & item == 'CFT_3_12' ~ 1, 
      raw == 'd' & item == 'CFT_3_13' ~ 1, raw == 'e' & item == 'CFT_3_14' ~ 1, raw == 'a' & item == 'CFT_3_15' ~ 1, 
      raw == 'd' & item == 'CFT_4_1' ~ 1, raw == 'b' & item == 'CFT_4_2' ~ 1, raw == 'e' & item == 'CFT_4_3' ~ 1, 
      raw == 'b' & item == 'CFT_4_4' ~ 1, raw == 'c' & item == 'CFT_4_5' ~ 1, raw == 'a' & item == 'CFT_4_6' ~ 1, 
      raw == 'd' & item == 'CFT_4_7' ~ 1, raw == 'a' & item == 'CFT_4_8' ~ 1, raw == 'a' & item == 'CFT_4_9' ~ 1, 
      raw == 'c' & item == 'CFT_4_10' ~ 1, raw == 'e' & item == 'CFT_4_11' ~ 1, TRUE ~ 0
    )
  ) %>% summarise(
    CFT_total = sum(score)
  )

# PSY_BOKI_DEMO_Neu & PSY_BOKI_DEMO
df.demo = df %>% filter(questionnaire == "PSY_BOKI_DEMO") %>%
  mutate(
    item = recode(item, 
                  `PSY_PIPS_Demo_Alter/age` = "age",
                  `PSY_PIPS_Demo_Geschlecht/gender` = "gender",
                  `PSY_BOKI_DEMO_1` = "cis",
                  `PSY_BOKI_DEMO_2` = "BPD",
                  `PSY_BOKI_DEMO_3` = "sinceBPD",
                  `PSY_PIPS_Demo_Diagnose/diagnosis` = "ASD",
                  `PSY_PIPS_Demo_seit/since` = "sinceASD",
                  `PSY_PIPS_DEMO_Family` = "ASDfamily",
                  `PSY_PIPS_DEMO_DISEASES_1` = "physDisease",
                  `PSY_PIPS_DEMO_MEDICATION` = "meds", 
                  `BOKI_Demo_1` = "PRN", # Bedarfsmedikation
                  `BOKI_DEMO_2` = "lastPRN", # letzte Einnahme
                  `BOKI_DEMO_3` = "therapy", # Psychotherapie im letzten halben Jahr
                  `PSY_BOKI_DEMO_4` = "pacemaker",
                  `PSY_PIPS_Demo_Rauchen/smoking` = "smoking",
                  `PSY_PIPS_Demo_fallsja_1` = "smoking_desc",
                  `PSY_PIPS_Demo_Alkohol/alcohol` = "alc",
                  `PSY_PIPS_Demo_fallsja_2` = "alc_desc",
                  `PSY_PIPS_Demo_Drogen/drugs` = "drugs",
                  `PSY_PIPS_Demo_fallsja_3` = "drugs_desc",
                  `PSY_PIPS_Demo_Sehhilfe/glasses` = "vision", 
                  `PSY_PIPS_DEMO_SCHREIBHAND` = "handedness",
                  `PSY_BOKI_DEMO_5` = "hobbies"),
    value = case_when(
      grepl("keine|nein", value, ignore.case = TRUE) ~ "0",
      grepl("ja", value, ignore.case = TRUE) ~ "1",
      grepl("FEMALE", value, ignore.case = TRUE) ~ "fem",
      grepl("MALE", value, ignore.case = TRUE) ~ "mal",
      grepl("DIVERS", value, ignore.case = TRUE) ~ "other",
      TRUE ~ value
    )
  )

df.demo = df.demo %>%
  group_by(SID) %>% select(SID, item, value) %>%
  distinct() %>%
  pivot_wider(names_from = item, values_from = value)

# PSY_BOKI_MWT
df.mwt = df %>% filter(questionnaire == "PSY_BOKI_MWT") %>%
  group_by(SID) %>% select(SID, numericValue) %>%
  mutate(
    numericValue = as.numeric(numericValue)
  ) %>%
  summarise(
    MWT_total = sum(numericValue, na.rm = T)
  )

# PSY_BOKI_ADC
df.adc = df %>% filter(questionnaire == "PSY_BOKI_ADC" & (section == "Abschnitt 2" | section == "Abschnitt 3")) %>%
  group_by(SID, section) %>%
  summarise(
    numericValue = sum(as.numeric(numericValue), na.rm = T)
  ) %>%
  ungroup() %>%
  pivot_wider(values_from = numericValue, names_from = section) %>%
  rename(
    "adc_p1" = `Abschnitt 2`,
    "adc_p2" = `Abschnitt 3`
  ) %>%
  mutate(
    adc_total = adc_p1 + adc_p2
  )

# BOKI_Rapport_Fragebogen
df.rap = df %>% filter(questionnaire == "BOKI_Rapport_Fragebogen") %>%
  mutate(
    numericValue = as.numeric(numericValue)
  ) %>% select(SID, item, numericValue) %>%
  pivot_wider(values_from = numericValue, names_from = item) %>%
  group_by(SID) %>%
  summarise(
    rapport = RAPP_1 + RAPP2 + RAPP3 + RAPP4 + RAPP5,
    video   = RAPP6,
    plexi   = RAPP7
  )

# PSY_BOKI_SPF
df.spf = df %>% filter(questionnaire == "PSY_BOKI_SPF") %>%
  select(SID, numericValue, item) %>%
  mutate(numericValue = as.numeric(numericValue)) %>%
  pivot_wider(values_from = numericValue, names_from = item) %>%
  mutate(
    spf_e = PSY_PIPS_SPF_Q1 + PSY_PIPS_SPF_Q4 + PSY_PIPS_SPF_Q6  + PSY_PIPS_SPF_Q8,
    spf_f = PSY_PIPS_SPF_Q2 + PSY_PIPS_SPF_Q5 + PSY_PIPS_SPF_Q9  + PSY_PIPS_SPF_Q11,
    spf_p = PSY_PIPS_SPF_Q3 + PSY_PIPS_SPF_Q7 + PSY_PIPS_SPF_Q10 + PSY_PIPS_SPF_Q12,
    spf_total = spf_e + spf_f + spf_p
  ) %>%
  select(SID, spf_e, spf_f, spf_p, spf_total)

# PSY_BOKI_BSL-23
df.bsl = df %>% filter(questionnaire == "PSY_BOKI_BSL-23") %>%
  mutate(
    numericValue = case_when(
      is.na(numericValue) ~ value,
      TRUE ~ numericValue
    ),
    numericValue = as.numeric(numericValue),
    section = case_when(
      item == "PSY_BOKI_BSL-23_Schieber" ~ "state",
      substr(item,16,16) == "_" ~ "sym", 
      substr(item,16,16) == "." ~ "beh"
    )
  ) %>% select(SID, section, numericValue) %>%
  group_by(SID, section) %>%
  summarise(
    BSL = sum(numericValue, na.rm = T)
  ) %>%
  pivot_wider(values_from = BSL, names_from = section, names_prefix = "BSL_")

# PSY_BOKI_TAS
df.tas = df %>% filter(questionnaire == "PSY_BOKI_TAS") %>%
  mutate(
    item = as.numeric(gsub("PSY_BOKI_TAS_", "", item)),
    numericValue = as.numeric(numericValue)
  ) %>%
  select(SID, item, numericValue)

# some need to be turned around
idx = c(4, 5, 10, 18, 19)
df.tas[df.tas$item %in% idx,]$numericValue = abs(df.tas[df.tas$item %in% idx,]$numericValue - 6)

df.tas = df.tas %>%
  group_by(SID) %>%
  summarise(
    tas_total = sum(numericValue, na.rm = T)
  )

# PSY_BOKI_SMS
df.sms = df %>% filter(questionnaire == "PSY_BOKI_SMS") %>%
  mutate(
    numericValue = case_when(
      value == "wahr" ~ 1,
      value == "falsch" ~ 0
    ), 
    item = as.numeric(gsub("PSY_BOKI_SMS_", "", item))
  ) %>% select(SID, item, numericValue)

# some need to be turned around
idx = c(1, 2, 3, 4, 9, 12, 14, 17, 20, 21, 22, 23)
df.sms[df.sms$item %in% idx,]$numericValue = abs(df.sms[df.sms$item %in% idx,]$numericValue - 1)

df.sms = df.sms %>%
  group_by(SID) %>%
  summarise(
    SMS_total = sum(numericValue, na.rm = T)
  )

# PSY_BOKI_AQ
df.aq = df %>% filter(questionnaire == "PSY_BOKI_AQ") %>%
  mutate(
    numericValue = as.numeric(case_when(
      substr(value,1,3) == "(1)" ~ 0,
      substr(value,1,3) == "(2)" ~ 0,
      substr(value,1,3) == "(3)" ~ 1,
      substr(value,1,3) == "(4)" ~ 1
    )),
    item = as.numeric(gsub("PSY_PIPS_AQ_Q", "", item))
  ) %>% select(SID, item, numericValue)

# some need to be turned around
idx = c(2, 4, 5, 6, 7, 9, 12, 13, 16, 18, 19, 20, 21, 22, 23, 26, 33, 35, 39, 41, 42, 43, 45, 46)
df.aq[df.aq$item %in% idx,]$numericValue = abs(df.aq[df.aq$item %in% idx,]$numericValue - 1)

df.aq = df.aq %>%
  group_by(SID) %>%
  summarise(
    aq_total = sum(numericValue, na.rm = T)
  )

# merge all together
ls.df = list(df.demo, df.cft, df.mwt, df.bdi, df.bsl, df.adc, df.aq, df.rap, df.sms, df.spf, df.tas)
df.sub = ls.df %>% reduce(full_join, by = "SID") 

# add iq scores
mwt = read_delim("MWT-norms.csv", show_col_types = F, delim = ";")
cft = read_delim("CFT-norms.csv", show_col_types = F, delim = ";")

df.sub$CFT_iq = NA
df.sub$MWT_iq = NA
for (i in 1:nrow(df.sub)) {
  if (df.sub$CFT_total[i] >= 9 & !is.na(df.sub$CFT_total[i]) & !is.na(df.sub$age[i]) & df.sub$age[i] >= 16 & df.sub$age[i] <= 60) {
    df.sub$CFT_iq[i] = cft[(df.sub$age[i] >= cft$lower & df.sub$age[i] <= cft$upper & df.sub$CFT_total[i] == cft$raw),]$iq
  }
  if (!is.na(df.sub$MWT_total[i])) {
    df.sub$MWT_iq[i] = mwt[(df.sub$MWT_total[i] == mwt$raw),]$iq
  }
}

write_csv(df.sub, file = "BOKI_centraXX.csv")
