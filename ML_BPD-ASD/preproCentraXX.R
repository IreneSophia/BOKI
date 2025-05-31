# Load packages 
library(tidyverse)
library(readxl)

# Clear global environment
rm(list=ls())
setwd("/media/emba/emba-2/ML_BOKI/demoCentraXX")

# load raw data
# columns of Interest: internalStudyMemberID, name2, code, value, section, (valueIndex), numericValue
df = c("BOKI_01-08_demo-old.csv", "BOKI_20240925.csv") %>%
  map_df(~read_delim(., show_col_types = F, locale = locale(encoding = "UTF-8"))) %>%
  select(internalStudyMemberID, name2, code, value, section, numericValue) %>%
  filter(internalStudyMemberID != "NEVIA_test" & !is.na(name2) & substr(internalStudyMemberID,1,10) != "BOKI_Pilot") %>%
  rename("questionnaire" = "name2", 
         "item" = "code", 
         "ID" = "internalStudyMemberID") %>%
  mutate(
    value = str_replace(value, ",", ";"),
    questionnaire = str_replace(questionnaire, "PSY_BOKI_DEMO_Neu", "PSY_BOKI_DEMO")
  ) %>%
  group_by(ID) %>% distinct()

## preprocess each questionnaire separately:

# PSY_BOKI_BDI 
# (10 bis 19: leichtes depressives Syndrom, 20 bis 29: mittelgradiges, >= 30: schweres)
df.bdi = df %>% filter(questionnaire == "PSY_BOKI_BDI") %>% 
  group_by(ID) %>%
  mutate(
    PSY_BOKI_BDI_V = if_else(value == "JA", 1, 0),
    PSY_BOKI_BDI_V = sum(PSY_BOKI_BDI_V, na.rm = T)
  ) %>%
  filter(item != "PSY_BOKI_BDI_V") %>%
  ungroup() %>%
  mutate(
    numericValue = case_when(
      # if they answered deliberate weight loss with yes, ignore item S
      item == "PSY_BOKI_BDI_S" & PSY_BOKI_BDI_V == 1 ~ 0,
      # if there are multiple answers chosen, select the highest value
      grepl(", ", numericValue, fixed = T) ~ max(readr::parse_number(str_split(numericValue, ", ")[[1]])),
      # if not, just convert it to a number
      T ~ as.numeric(numericValue)
    ),
    # fix a mistake in CentraXX which codes one item wrong
    numericValue = case_when(
      item == "PSY_BOKI_BDI_F" & numericValue > 2 ~ numericValue - 1,
      T ~ numericValue
    )
  ) %>%
  select(questionnaire, ID, item, numericValue) %>%
  group_by(ID) %>%
  summarise(
    BDI_total = sum(numericValue, na.rm = T)
  )

# PSY_BOKI_CFT
df.cft = df %>% filter(questionnaire == "PSY_BOKI_CFT") %>% 
  group_by(ID) %>%
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
  group_by(ID) %>% select(ID, item, value) %>%
  distinct() %>%
  pivot_wider(names_from = item, values_from = value) %>%
  mutate(
    gender = case_when(
      # fill in missing gender values from database
      ID == "BOKI_49_R" | ID == "BOKI_10_R" ~ "fem",
      T ~ gender
    ),
    ASD = case_when(
      is.na(ASD) | ASD == "0" ~ 0,
      ID == "BOKI_35_L" ~ 0, # mistake, checked original data of participant
      T ~ 1
    ),
    BPD = case_when(
      is.na(BPD) | BPD == "0" ~ 0,
      T ~ 1
    ), 
    age = case_when(
      ID == "BOKI_49_R" ~ 24,
      ID == "BOKI_51_L" ~ 21,
      T ~ as.numeric(gsub("\\D", "", age))
    )
  )

# PSY_BOKI_MWT
df.mwt = df %>% filter(questionnaire == "PSY_BOKI_MWT") %>%
  group_by(ID) %>% select(ID, numericValue) %>%
  mutate(
    numericValue = as.numeric(numericValue)
  ) %>%
  summarise(
    MWT_total = sum(numericValue, na.rm = T)
  )

# PSY_BOKI_ADC
df.adc = df %>% filter(questionnaire == "PSY_BOKI_ADC" & (section == "Abschnitt 2" | section == "Abschnitt 3")) %>%
  group_by(ID, section) %>%
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
    ADC_total = adc_p1 + adc_p2
  )

# BOKI_Rapport_Fragebogen
df.rap = df %>% filter(questionnaire == "BOKI_Rapport_Fragebogen") %>%
  mutate(
    numericValue = as.numeric(numericValue)
  ) %>% select(ID, item, numericValue) %>%
  pivot_wider(values_from = numericValue, names_from = item) %>%
  group_by(ID) %>%
  summarise(
    rapport = RAPP_1 + RAPP2 + RAPP3 + RAPP4 + RAPP5,
    video   = RAPP6,
    plexi   = RAPP7
  )

# PSY_BOKI_SPF
df.spf = df %>% filter(questionnaire == "PSY_BOKI_SPF") %>%
  select(ID, numericValue, item) %>%
  mutate(numericValue = as.numeric(numericValue)) %>%
  pivot_wider(values_from = numericValue, names_from = item) %>%
  mutate(
    spf_e = PSY_PIPS_SPF_Q1 + PSY_PIPS_SPF_Q4 + PSY_PIPS_SPF_Q6  + PSY_PIPS_SPF_Q8,
    spf_f = PSY_PIPS_SPF_Q2 + PSY_PIPS_SPF_Q5 + PSY_PIPS_SPF_Q9  + PSY_PIPS_SPF_Q11,
    spf_p = PSY_PIPS_SPF_Q3 + PSY_PIPS_SPF_Q7 + PSY_PIPS_SPF_Q10 + PSY_PIPS_SPF_Q12,
    SPF_total = spf_e + spf_f + spf_p
  ) %>%
  select(ID, spf_e, spf_f, spf_p, SPF_total)

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
      substr(item,16,16) == "_" ~ "total", 
      substr(item,16,16) == "." ~ "beh"
    )
  ) %>% select(ID, section, numericValue) %>%
  group_by(ID, section) %>%
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
  select(ID, item, numericValue)

# some need to be turned around
idx = c(4, 5, 10, 18, 19)
df.tas[df.tas$item %in% idx,]$numericValue = abs(df.tas[df.tas$item %in% idx,]$numericValue - 6)

df.tas = df.tas %>%
  group_by(ID) %>%
  summarise(
    TAS_total = sum(numericValue, na.rm = T)
  )

# PSY_BOKI_SMS
df.sms = df %>% filter(questionnaire == "PSY_BOKI_SMS") %>%
  mutate(
    numericValue = case_when(
      value == "wahr" ~ 1,
      value == "falsch" ~ 0
    ), 
    item = as.numeric(gsub("PSY_BOKI_SMS_", "", item))
  ) %>% select(ID, item, numericValue)

# some need to be turned around
idx = c(1, 2, 3, 4, 9, 12, 14, 17, 20, 21, 22, 23)
df.sms[df.sms$item %in% idx,]$numericValue = abs(df.sms[df.sms$item %in% idx,]$numericValue - 1)

df.sms = df.sms %>%
  group_by(ID) %>%
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
  ) %>% select(ID, item, numericValue)

# some need to be turned around
idx = c(2, 4, 5, 6, 7, 9, 12, 13, 16, 18, 19, 20, 21, 22, 23, 26, 33, 35, 39, 41, 42, 43, 45, 46)
df.aq[df.aq$item %in% idx,]$numericValue = abs(df.aq[df.aq$item %in% idx,]$numericValue - 1)

df.aq = df.aq %>%
  group_by(ID) %>%
  summarise(
    AQ_total = sum(numericValue, na.rm = T)
  )

# merge all together
ls.df = list(df.demo, df.cft, df.mwt, df.bdi, df.bsl, df.adc, df.aq, df.rap, df.sms, df.spf, df.tas)
df.sub = ls.df %>% reduce(full_join, by = "ID") 

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

# add dyad to the information
df.sub = df.sub %>% 
  filter(substr(ID, 1, 5) == "BOKI_") %>%
  mutate(
    dyad = substr(ID, 1, 7), 
    # IQ test done on a different day due to last minute scheduling
    CFT_iq = if_else(ID == "BOKI_43_R", 117, CFT_iq)
  ) 

# add whether participants were used in the analysis
df.sub = df.sub %>% 
  filter(dyad %in% c(
    "BOKI_08", "BOKI_10", "BOKI_13", "BOKI_14", "BOKI_15", "BOKI_16", "BOKI_17", 
    "BOKI_19", "BOKI_24", "BOKI_25", "BOKI_26", "BOKI_27", "BOKI_28", "BOKI_29", 
    "BOKI_34", "BOKI_36", "BOKI_37", "BOKI_40", "BOKI_42", "BOKI_43", "BOKI_49", 
    "BOKI_50", "BOKI_51", "BOKI_53", "BOKI_54", "BOKI_57", "BOKI_58", "BOKI_59", 
    "BOKI_61")) %>%
  select(ID, dyad, age, gender, BPD, ASD, CFT_iq, MWT_iq, BDI_total, BSL_total,
         ADC_total, AQ_total, SMS_total, SPF_total, TAS_total,
         rapport, video)

# combine with old dataset and labels
df.speech = read_csv("demo_MLSPE_tt.csv") %>%
  mutate(
    gender = case_when(
      gender == 0 ~ 'mal', 
      gender == 1 ~ 'fem'
    )
  )
df.rapport = read_excel("rapport_corona_raw.xlsx") %>%
  group_by(ID) %>%
  summarise(
    rapport = Sympathie + Freundlichkeit + Wohlbefinden + Reibungslosigkeit + Interaktion,
    video   = Videoeinfluss,
    plexi   = Plexiglas
  )

df.sub = rbind(df.sub, 
               merge(df.speech, df.rapport))
  
# add the dyad labels
df.sub = df.sub %>%
  group_by(dyad) %>%
  mutate(
    BPD.dyad = sum(BPD),
    ASD.dyad = sum(ASD)
  ) %>% ungroup() %>% 
  mutate(
    label = case_when(
      BPD.dyad == 0 & ASD.dyad == 0 ~ "COMP-COMP",
      BPD.dyad == 0 & ASD.dyad == 1 ~ "ASD-COMP",
      T ~ "BPD-COMP",
    )
  ) %>%
  relocate(dyad, ID, label) %>%
  select(-BPD.dyad, -ASD.dyad)

write_csv(df.sub, file = "BOKI_centraXX.csv")
