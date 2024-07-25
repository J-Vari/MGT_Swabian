# load libraries for cleaning #

library (dplyr)
library(broom)
library(tidyverse)
library(corrr)
library(ggcorrplot)
library(car)
library(readr)
library(here)
library(stringr)
library(tidytext)
options(scipencount=999)

# Task data
## read task data
audio_eval_male <- read_delim(here("raw_data", "data_exp_86579-v59_task-ue28.csv"), col_names = TRUE, delim = ",")
audio_eval_female <- read_delim(here("raw_data", "data_exp_86579-v59_task-v697.csv"), col_names = TRUE, delim = ",")
audio_eval_male_vs_female <-read_delim(here("raw_data", "data_exp_86579-v59_task-mdix.csv"), col_names = TRUE, delim = ",")

origin_authen_female <-read_delim(here("raw_data", "data_exp_86579-v59_task-8u1p.csv"), col_names = TRUE, delim = ",")
origin_authen_male_vs_female <-read_delim(here("raw_data", "data_exp_86579-v59_task-r61o.csv"), col_names = TRUE, delim = ",")


# clean tasks male only
## Audio evaluation
## select relevant columns for audio evaluations & rename
audio_eval_male%>%
  select(`Participant Public ID`, `UTC Date and Time`, `UTC Timestamp`, `Response Type`, `Response`, `Object Name`, `Spreadsheet: Audio`, `Spreadsheet: Speaker`, `Spreadsheet: Speaker gender`, `Spreadsheet: Variety`,
         `Spreadsheet: Political orientation`, `Spreadsheet: Stimulus number`, `Spreadsheet: Stimulus1`, `Spreadsheet: Stimulus2`, `Spreadsheet: Stimulus3`, `Spreadsheet: Stimulus4`, `Spreadsheet: Stimulus5`, 
         `Spreadsheet: Stimulus6`, `Spreadsheet: Attention Check`, `Store: Attention Check`)%>%
  rename(Audio_Track = `Spreadsheet: Audio`, Speaker = `Spreadsheet: Speaker`, Speaker_Gender = `Spreadsheet: Speaker gender`, Lang.Variety_Audio = `Spreadsheet: Variety`,
         Polit_ori = `Spreadsheet: Political orientation`, Statement_no = `Spreadsheet: Stimulus number`, Att.check_acc = `Store: Attention Check`)%>%
  filter(`Response Type` == "response")%>%
  filter (str_starts(Speaker, "Exp"))->audio_eval_male

## remove markdown from values
audio_eval_male%>%
mutate(across(starts_with("Spreadsheet:"), ~ str_remove_all(., "\\*\\*")))-> audio_eval_male

## create one column with attribute related to rating
audio_eval_male%>%
mutate(Attribute = case_when(
  `Object Name` == "RatingScale_Stimulus1" ~ `Spreadsheet: Stimulus1`,
  `Object Name` == "RatingScale_Stimulus2" ~ `Spreadsheet: Stimulus2`,
  `Object Name` == "RatingScale_Stimulus3" ~ `Spreadsheet: Stimulus3`,
  `Object Name` == "RatingScale_Stimulus4" ~ `Spreadsheet: Stimulus4`,
  `Object Name` == "RatingScale_Stimulus5" ~ `Spreadsheet: Stimulus5`,
  `Object Name` == "RatingScale_Stimulus6" ~ `Spreadsheet: Stimulus6`,
  `Object Name` == "RatingScale_AttentionCheck" ~ `Spreadsheet: Attention Check`))->audio_eval_male

## delete unnecessary columns & elaborate levels of categorical variables
audio_eval_male%>%
  select(-starts_with("Spreadsheet:"), -`Response Type`)%>%
  mutate(Speaker_Gender = ifelse(Speaker_Gender=="f", "female", "male"))%>%
  mutate (Polit_ori = ifelse(Polit_ori == "l", "left", "right"))%>%
  mutate (Lang.Variety_Audio = ifelse(Lang.Variety_Audio == "st", "Standard German", "Regional Variety"))-> audio_eval_male

## Identify-final-accuracy
## select final response attention check accuracy
audio_eval_male %>%
  group_by(`Participant Public ID`) %>%
      mutate(Att.check_acc_last = last(Att.check_acc)) -> audio_eval_male

audio_eval_male %>%
  select(-Att.check_acc) %>%
  rename(Att.check_acc = Att.check_acc_last) -> audio_eval_male

## duplicates & data loss in audio_eval_male?
audio_eval_male%>%
  group_by(`Participant Public ID`)%>%
  count()-> pp_rows # 3 pp not normal row no of 168

audio_eval_male%>%
  filter (`Participant Public ID` == "281093394820202" | `Participant Public ID` == "281348585989177" | `Participant Public ID` =="281427437681691")%>%
  group_by (Audio_Track, Attribute)%>%
  summarise(n_distinct(Attribute))-> pp_audio_check # no repeated attributes

audio_eval_male%>%
  filter (`Participant Public ID` == "281093394820202" | `Participant Public ID` == "281348585989177" | `Participant Public ID` =="281427437681691")%>%
  group_by (`Participant Public ID`, Attribute, Audio_Track)%>%
  summarise(n_distinct(`Participant Public ID`,  Attribute, Audio_Track))-> pp_audio_check # no repeated attributes

audio_eval_male%>%
  distinct()->audio_eval_male_test # same row no. -> no duplicate rows


# clean male only
## Origin & authenticity

origin_authen_male <-read_delim(here("raw_data", "data_exp_86579-v59_task-brwb.csv"), col_names = TRUE, delim = ",")

origin_authen_male%>%
  select(`Participant Public ID`, `Response Type`, Response, `Spreadsheet: Display`, `Spreadsheet: Audio`, `Spreadsheet: Variety`, `Object ID`, `UTC Date and Time`, `UTC Timestamp`)%>%
  rename(Target = `Spreadsheet: Display`, Audio = `Spreadsheet: Audio`, Variety = `Spreadsheet: Variety`)%>%
  filter (`Response Type` == "response")%>%
  mutate(Target_Concept = case_when(
    Target == "Political level" ~ "Political level",
    Target == "Paradigm check" ~ "Paradigm check",
    Target == "Origin & authenticity" & grepl("^[0-9]+$", Response) & Variety == "non-st" ~ "proximity_variety_non-st",
    Target == "Origin & authenticity" & grepl("^[0-9]+$", Response) & Variety == "st" ~ "proximity_variety_st",
    Target == "Origin & authenticity" & !grepl("^[0-9]+$", Response) & Variety == "st" ~ "identity_lang.variety_st",
    Target == "Origin & authenticity" & !grepl("^[0-9]+$", Response) & Variety == "non-st" ~ "identity_lang.variety_non-st",
    TRUE ~ NA_character_))%>%
  filter(Response != "continue")%>%
  select(-`Response Type`, -Target)%>%
  select(`Participant Public ID`, Response, Target_Concept, Audio, Variety, `Object ID`, `UTC Date and Time`, `UTC Timestamp`)-> origin_authen_male


## separate Paradigm Check Level
origin_authen_male %>%
  mutate(
    Target_Concept = case_when(
      `Object ID` == "object-1145" ~ "Paradigm check 1",
      `Object ID` == "object-1146" ~ "Paradigm check 2",
      TRUE ~ Target_Concept)) -> origin_authen_male


# Identify those participants that were not deceived by the speaker illusion
## Preprocess answers to open text fields
origin_authen_male %>%
  mutate(Response_tidy = str_to_lower(Response)) -> origin_authen_male

## Create new variable (1 = deceived, 0 = not deceived)
origin_authen_male %>%
  mutate(deceived = ifelse(str_detect(Response_tidy, "(gleich|selb).{0,2} (person|sprecher|sprechende).*") |
           str_detect(Response_tidy, "(stimm).{0,2} (verstell).*"), 0, 1)) -> origin_authen_male

origin_authen_male %>%
  group_by(`Participant Public ID`) %>%
  mutate(deceived = case_when(
    any(deceived == 0) ~ 0,
    TRUE ~ deceived)) %>%
  ungroup() %>% 
  select(-Response_tidy) -> origin_authen_male

## check unique identification of each response

origin_authen_male%>%
  group_by(`Participant Public ID`, Audio,
                  Variety, Target_Concept) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1L)-> duplicates # 4 pp duplicated

## Check reasons for duplicates

origin_authen_male%>%
  filter (`Participant Public ID` == "281162358237373" | `Participant Public ID` == "281082304369845" | `Participant Public ID` == "281251938576386")-> check_pp
## => double ratings/ duplicated coz pp changed their rating at a later time OR pp selected "__other" and added answer to open Q field


## Solution: create 2 new columns
### 1) select later response of pp depending on Time Stamp

origin_authen_male %>%
  group_by(`Participant Public ID`, Audio, Target_Concept) %>%
  mutate(Response_corrected = Response[which.max(`UTC Timestamp`)])->origin_authen_male

## 2) if "_other" selected, next line text box entry into new column, then drop duplicated "_other"

origin_authen_male%>%
  mutate(Response_Text_identity_lang.var = ifelse(Response == "__other", lead(Response), NA))%>%
  ungroup()%>%
  filter(!(Response_corrected == "__other" & is.na(Response_Text_identity_lang.var)))%>%
  distinct(`Participant Public ID`, Audio, Target_Concept, Variety, .keep_all = TRUE)%>%
  select(-Response, -`Object ID`,-`UTC Date and Time`, -`UTC Timestamp`)->origin_authen_male


## check duplicates now
origin_authen_male%>%
  group_by(`Participant Public ID`, Audio,
           Variety, Target_Concept) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1L)-> duplicates


## turn long into wide format to match 3 different data sets
pivot_wider(origin_authen_male , names_from = "Target_Concept",
              values_from = "Response_corrected")->origin_authen_male

## fill NAs with repeated values in new columns

# Explicitly define the new columns to be filled
new_columns_to_fill <- c("Response_Text_identity_lang.var", "Political level",
                         "identity_lang.variety_non-st", "identity_lang.variety_st",
                         "Paradigm check 1", "Paradigm check 2")

# Fill missing values in the specified columns within each participant
 origin_authen_male %>%
  group_by(`Participant Public ID`) %>%
  fill(all_of(new_columns_to_fill), .direction = "downup") %>%
  ungroup()-> origin_authen_male


## Combine 2 data sets: audio eval, origin & authenticity

audio_eval_male %>%
   left_join(origin_authen_male, 
             by = c("Participant Public ID" = "Participant Public ID", 
                    "Audio_Track" = "Audio"))%>%
  select(-Variety)-> combined_data


# add questionnaire data
combined_data%>%
  

 

# clean tasks female only
## Audio eval female
## select relevant columns for audio evaluations & rename
audio_eval_female%>%
  select(`Participant Public ID`, `Response Type`, `Response`, `Object Name`, `Spreadsheet: Audio`, `Spreadsheet: Speaker`, `Spreadsheet: Speaker gender`, `Spreadsheet: Variety`,
         `Spreadsheet: Political orientation`, `Spreadsheet: Stimulus number`, `Spreadsheet: Stimulus1`, `Spreadsheet: Stimulus2`, `Spreadsheet: Stimulus3`, `Spreadsheet: Stimulus4`, `Spreadsheet: Stimulus5`, 
         `Spreadsheet: Stimulus6`, `Spreadsheet: Attention Check`, `Store: Attention Check`)%>%
  rename(Audio_Track = `Spreadsheet: Audio`, Speaker = `Spreadsheet: Speaker`, Speaker_Gender = `Spreadsheet: Speaker gender`, Lang.Variety_Audio = `Spreadsheet: Variety`,
         Polit_ori = `Spreadsheet: Political orientation`, Statement_no = `Spreadsheet: Stimulus number`, Att.check_acc = `Store: Attention Check`)%>%
  filter(`Response Type` == "response")%>%
  filter (str_starts(Speaker, "Exp"))->audio_eval_female

## remove markdown from values
audio_eval_female%>%
  mutate(across(starts_with("Spreadsheet:"), ~ str_remove_all(., "\\*\\*")))-> audio_eval_female

## create one column with attribute related to rating
audio_eval_female%>%
  mutate(Attribute = case_when(
    `Object Name` == "RatingScale_Stimulus1" ~ `Spreadsheet: Stimulus1`,
    `Object Name` == "RatingScale_Stimulus2" ~ `Spreadsheet: Stimulus2`,
    `Object Name` == "RatingScale_Stimulus3" ~ `Spreadsheet: Stimulus3`,
    `Object Name` == "RatingScale_Stimulus4" ~ `Spreadsheet: Stimulus4`,
    `Object Name` == "RatingScale_Stimulus5" ~ `Spreadsheet: Stimulus5`,
    `Object Name` == "RatingScale_Stimulus6" ~ `Spreadsheet: Stimulus6`,
    `Object Name` == "RatingScale_AttentionCheck" ~ `Spreadsheet: Attention Check`))->audio_eval_female

## delete unnecessary columns & elaborate levels of categorical variables
audio_eval_female%>%
  select(-starts_with("Spreadsheet:"), -`Response Type`)%>%
  mutate(Speaker_Gender = ifelse(Speaker_Gender=="FALSE", "female", "male"))%>%
  mutate (Polit_ori = ifelse(Polit_ori == "l", "left", "right"))%>%
  mutate (Lang.Variety_Audio = ifelse(Lang.Variety_Audio == "st", "Standard German", "Regional Variety"))-> audio_eval_female

## select final response attention check accuracy
audio_eval_female %>%
  group_by(`Participant Public ID`) %>%
  mutate(Att.check_acc_last = last(Att.check_acc)) -> audio_eval_female

audio_eval_female %>%
  select(-Att.check_acc) %>%
  rename(Att.check_acc = Att.check_acc_last) -> audio_eval_female

## clean origin_authen_female & elaborate Origin & authenticity column
origin_authen_female%>%
  select(`Participant Public ID`, `Response Type`, Response, `Spreadsheet: Display`, `Spreadsheet: Audio`, `Spreadsheet: Variety`, `Object ID`)%>%
  rename(Target = `Spreadsheet: Display`, Audio = `Spreadsheet: Audio`, Variety = `Spreadsheet: Variety`)%>%
  filter (`Response Type` == "response")%>%
  mutate(Target_Concept = case_when(
    Target == "Political level" ~ "Political level",
    Target == "Paradigm check" ~ "Paradigm check",
    Target == "Origin & authenticity" & grepl("^[0-9]+$", Response) ~ "proximity_standard.variety",
    Target == "Origin & authenticity" & !grepl("^[0-9]+$", Response) ~ "identity_lang.variety",
    TRUE ~ NA_character_))%>%
  filter(Response != "continue")%>%
  select(-`Response Type`, -Target) %>%
  select(`Participant Public ID`, Response, Target_Concept, Audio, Variety, `Object ID`) -> origin_authen_female

## separate Paradigm Check Level
origin_authen_female %>%
  mutate(
    Target_Concept = case_when(
      `Object ID` == "object-1145" ~ "Paradigm check 1",
      `Object ID` == "object-1146" ~ "Paradigm check 2",
      TRUE ~ Target_Concept)) %>%
  select(-`Object ID`) -> origin_authen_female

# Identify those participants that were not deceived by the speaker illusion
## Preprocess answers to open text fields
origin_authen_female %>%
  mutate(Response_tidy = str_to_lower(Response)) -> origin_authen_female

## Create new variable (1 = deceived, 0 = not deceived)
origin_authen_female %>%
  mutate(deceived = ifelse(str_detect(Response_tidy, "(gleich|selb).{0,2} (person|sprecher|sprechende).*") |
                             str_detect(Response_tidy, "(stimm).{0,2} (verstell).*"), 0, 1)) -> origin_authen_female

origin_authen_female %>%
  group_by(`Participant Public ID`) %>%
  mutate(deceived = case_when(
    any(deceived == 0) ~ 0,
    TRUE ~ deceived)) %>%
  ungroup() %>% 
  select(-Response_tidy) -> origin_authen_female

## turn long into wide format to match 3 different data sets
pivot_wider(origin_authen_female, names_from = "Target_Concept",
            values_from = "Response", values_fill = NA)->origin_authen_female


# *clean tasks male vs. female*
## Audio Eval male vs. female
## select relevant columns for audio evaluations & rename
audio_eval_male_vs_female%>%
  select(`Participant Public ID`, `Response Type`, `Response`, `Object Name`, `Spreadsheet: Audio`, `Spreadsheet: Speaker`, `Spreadsheet: Speaker gender`, `Spreadsheet: Variety`,
         `Spreadsheet: Political orientation`, `Spreadsheet: Stimulus number`, `Spreadsheet: Stimulus1`, `Spreadsheet: Stimulus2`, `Spreadsheet: Stimulus3`, `Spreadsheet: Stimulus4`, `Spreadsheet: Stimulus5`, 
         `Spreadsheet: Stimulus6`, `Spreadsheet: Attention Check`, `Store: Attention Check`)%>%
  rename(Audio_Track = `Spreadsheet: Audio`, Speaker = `Spreadsheet: Speaker`, Speaker_Gender = `Spreadsheet: Speaker gender`, Lang.Variety_Audio = `Spreadsheet: Variety`,
         Polit_ori = `Spreadsheet: Political orientation`, Statement_no = `Spreadsheet: Stimulus number`, Att.check_acc = `Store: Attention Check`)%>%
  filter(`Response Type` == "response")%>%
  filter (str_starts(Speaker, "Exp"))->audio_eval_male_vs_female

## remove markdown from values
audio_eval_male_vs_female%>%
  mutate(across(starts_with("Spreadsheet:"), ~ str_remove_all(., "\\*\\*")))-> audio_eval_male_vs_female

## create one column with attribute related to rating
audio_eval_male_vs_female%>%
  mutate(Attribute = case_when(
    `Object Name` == "RatingScale_Stimulus1" ~ `Spreadsheet: Stimulus1`,
    `Object Name` == "RatingScale_Stimulus2" ~ `Spreadsheet: Stimulus2`,
    `Object Name` == "RatingScale_Stimulus3" ~ `Spreadsheet: Stimulus3`,
    `Object Name` == "RatingScale_Stimulus4" ~ `Spreadsheet: Stimulus4`,
    `Object Name` == "RatingScale_Stimulus5" ~ `Spreadsheet: Stimulus5`,
    `Object Name` == "RatingScale_Stimulus6" ~ `Spreadsheet: Stimulus6`,
    `Object Name` == "RatingScale_AttentionCheck" ~ `Spreadsheet: Attention Check`))->audio_eval_male_vs_female

## delete unnecessary columns & elaborate levels of categorical variables
audio_eval_male_vs_female%>%
  select(-starts_with("Spreadsheet:"), -`Response Type`) %>%
  mutate(Speaker_Gender = ifelse(Speaker_Gender=="f", "female", "male"))%>%
  mutate (Polit_ori = ifelse(Polit_ori == "l", "left", "right"))%>%
  mutate (Lang.Variety_Audio = ifelse(Lang.Variety_Audio == "st", "Standard German", "Regional Variety"))-> audio_eval_male_vs_female

## select final response attention check accuracy
audio_eval_male_vs_female %>%
  group_by(`Participant Public ID`) %>%
  mutate(Att.check_acc_last = last(Att.check_acc)) -> audio_eval_male_vs_female

audio_eval_male_vs_female %>%
  select(-Att.check_acc) %>%
  rename(Att.check_acc = Att.check_acc_last) -> audio_eval_male_vs_female

## clean origin_authen_male_vs_female & elaborate Origin & authenticity column
origin_authen_male_vs_female%>%
  select(`Participant Public ID`, `Response Type`, Response, `Spreadsheet: Display`, `Spreadsheet: Audio`, `Spreadsheet: Variety`, `Object ID`)%>%
  rename(Target = `Spreadsheet: Display`, Audio = `Spreadsheet: Audio`, Variety = `Spreadsheet: Variety`)%>%
  filter (`Response Type` == "response")%>%
  mutate(Target_Concept = case_when(
    Target == "Political level" ~ "Political level",
    Target == "Paradigm check" ~ "Paradigm check",
    Target == "Origin & authenticity" & grepl("^[0-9]+$", Response) ~ "proximity_standard.variety",
    Target == "Origin & authenticity" & !grepl("^[0-9]+$", Response) ~ "identity_lang.variety",
    TRUE ~ NA_character_))%>%
  filter(Response != "continue")%>%
  select(-`Response Type`, -Target) %>%
  select(`Participant Public ID`, Response, Target_Concept, Audio, Variety, `Object ID`)-> origin_authen_male_vs_female

## separate Paradigm Check Level
origin_authen_male_vs_female %>%
  mutate(
    Target_Concept = case_when(
      `Object ID` == "object-1145" ~ "Paradigm check 1",
      `Object ID` == "object-1146" ~ "Paradigm check 2",
      TRUE ~ Target_Concept)) %>%
  select(-`Object ID`) -> origin_authen_male_vs_female

# Identify those participants that were not deceived by the speaker illusion
## Preprocess answers to open text fields
origin_authen_male_vs_female %>%
  mutate(Response_tidy = str_to_lower(Response)) -> origin_authen_male_vs_female

## Create new variable (1 = deceived, 0 = not deceived)
origin_authen_male_vs_female %>%
  mutate(deceived = ifelse(str_detect(Response_tidy, "(gleich|selb).{0,2} (person|sprecher|sprechende).*") |
                             str_detect(Response_tidy, "(stimm).{0,2} (verstell).*"), 0, 1)) -> origin_authen_male_vs_female

origin_authen_male_vs_female %>%
  group_by(`Participant Public ID`) %>%
  mutate(deceived = case_when(
    any(deceived == 0) ~ 0,
    TRUE ~ deceived)) %>%
  ungroup() %>% 
  select(-Response_tidy) -> origin_authen_male_vs_female

## turn long into wide format to match 3 different data sets
pivot_wider(origin_authen_male_vs_female, names_from = "Target_Concept",
            values_from = "Response", values_fill = NA)->origin_authen_male_vs_female



# *Background Questionnaire*

## read data

questionnaire <- read_delim(here("raw_data", "data_exp_86579-v59_questionnaire-ry6q.csv"), col_names = TRUE, delim = ",")
as_tibble(questionnaire)
questionnaire

# get relevant columns'

questionnaire%>%
  filter(`Event Index` != "END OF FILE")%>%
  select ("Participant Public ID", "Participant Status", "UTC Timestamp", "Participant OS", "Participant Browser", "Task Name", "counterbalance-nimi", "counterbalance-x3xi", "counterbalance-xq3l", "randomiser-rtb5", "age", "gender", "state_of_residence", "education_school", "education_profession1", "education_profession2", "income", "profession", "profession-text", starts_with("languages_caregiver1"), starts_with("languages-caregiver2"), starts_with("political_"), "other_languages", "own_dialect", "own_dialect-quantised", "party", "party-text", "party-quantised", matches("^social_desirability_[A-I]-quantised$"), matches("^populism_[A-D]$"))-> questionnaire

# prep: assign classes
as.factor(questionnaire$`Participant Public ID`)->questionnaire$`Participant Public ID`
as.numeric(questionnaire$age)->questionnaire$age
as.factor(questionnaire$gender)->questionnaire$gender
as.factor(questionnaire$state_of_residence)->questionnaire$state_of_residence
as.factor(questionnaire$education_school)->questionnaire$education_school
as.factor(questionnaire$education_profession1)->questionnaire$education_profession1
as.factor(questionnaire$education_profession2)->questionnaire$education_profession2
as.factor(questionnaire$income)->questionnaire$income
as.factor(questionnaire$profession)->questionnaire$profession
as.numeric(questionnaire$political_spectrum)->questionnaire$political_spectrum
as.numeric(questionnaire$own_dialect)->questionnaire$own_dialect
as.factor(questionnaire$party)->questionnaire$party

# identify multiple participation#

questionnaire %>%
  group_by (`Participant Public ID`)%>%
  summarise (n_distinct (`UTC Timestamp`))-> multi_part 


# separate questionnaire into 2 data sets


## data set 1: technical stuff & demographics

questionnaire%>%
  select("Participant Public ID", "Participant Status", "Participant OS", "Participant Browser", "Task Name", "counterbalance-nimi", "counterbalance-x3xi", "counterbalance-xq3l", "randomiser-rtb5", "age", "gender", "state_of_residence", "education_school", "education_profession1", "education_profession2", "income", "profession", "profession-text")->pp_bckgr

pp_bckgr%>%
  ungroup()%>%
  summarise(n_distinct(`Participant Public ID`))# 1315

pp_bckgr%>%
  group_by (`Participant Public ID`)%>%
  count(`Participant Public ID`)-> pp_bckgr_rows # 1 per pp

## create new dummy variables education

pp_bckgr%>%
  mutate (education_profession1_sum = case_when
          (education_profession1 == "Berufsfachschulabschluss" ~ "low",
            education_profession1 == "Fachhochschulabschluss" ~ "high",
            education_profession1 == "Fachschulabschluss" ~ "middle",
            education_profession1 == "Hochschulabschluss (Bachelor)" ~ "high",
            education_profession1 == "Hochschulabschluss (Master)" ~ "high",
            education_profession1 == "Hochschulabschluss (Promotion)" ~ "high",
            education_profession1 == "Keinen" ~ "low",
            education_profession1 == "Meister/in, Technikerabschluss" ~ "middle",
            education_profession1 == "Teilfacharbeiterabschluss" ~ "low",
            education_profession1 == "Abgeschlossene gewerbliche oder landwirtschaftliche Lehre" ~ "middle",
            education_profession1 == "Abgeschlossene kaufmännische Lehre" ~ "middle",
            education_profession1 == "beruflich-betriebliche Anlernzeit mit Abschlusszeugnis, aber keine Lehre" ~ "low", TRUE ~ NA))-> pp_bckgr


pp_bckgr%>%
  mutate (education_school_sum = case_when
          (education_school == "Schule beendet ohne Abschluss"  ~ "low",
            education_school == "Hauptschulabschluss/ Volksschulabschluss/ Abschluss der polytechnischen Oberschule 8. oder 9. Klasse" ~ "low",
            education_school == "Realschulabschluss/ Mittlere Reife/ Fachschulreife oder Abschluss der polytechnischen Oberschule 10. Klasse" ~ "middle",
            education_school == "Fachhochschulreife (Abschluss einer Fachoberschule etc.)" ~ "high",
            education_school == "Abitur bzw. erweiterte Oberschule mit Abschluss 12. Klasse (Hochschulreife)" ~ "high",
            education_school == "bin noch Schüler/in" ~ "still at school",
            education_school == "andere" ~ "other",
            education_school == "NA" ~ "NA", TRUE ~ NA))-> pp_bckgr

pp_bckgr%>%
  group_by (`Participant Public ID`)%>%
  count(`Participant Public ID`)-> pp_bckgr_rows # 1 per pp

pp_bckgr%>%
  ungroup()%>%
  summarise(n_distinct(`Participant Public ID`))#1315

## create summary tables for pp_bckgr

## check randomization:

pp_bckgr%>%
  group_by(`randomiser-rtb5`,`counterbalance-nimi`, `counterbalance-x3xi`, `counterbalance-xq3l`)%>%
  summarise(n_distinct(`Participant Public ID`))->random


## check: participant background

### federal state

pp_bckgr%>%
  group_by(state_of_residence)%>%
  summarise(n_distinct(`Participant Public ID`))->pp_state

### participant age

pp_bckgr%>%
  ungroup()%>%
  drop_na(age)%>%
  summarise(mean(age), sd(age), min (age), max (age))->pp_age

### participant gender

pp_bckgr%>%
  group_by(gender)%>%
  summarise(n_distinct(`Participant Public ID`))->pp_gender

### sum_education_school
pp_bckgr%>%
  ungroup()%>%
  group_by(education_school_sum)%>%
  summarise(n_distinct(`Participant Public ID`))->sum_education

### education
pp_bckgr%>%
  group_by(education_school)%>%
  summarise(n_distinct(`Participant Public ID`))->education

### sum_profession
pp_bckgr%>%
  group_by(education_profession1_sum)%>%
  summarise(n_distinct(`Participant Public ID`))->sum_profession1


### income

pp_bckgr%>%
  group_by(income)%>%
  summarise(n_distinct(`Participant Public ID`))->income

### profession

pp_bckgr%>%
  group_by(profession)%>%
  summarise(n_distinct(`Participant Public ID`))->sum_profession



## data set 2: check specific IVs language & populism

questionnaire%>%
  select("Participant Public ID", starts_with("languages_caregiver1"), starts_with("languages_caregiver2"), "other_languages", "own_dialect", "party", "party-text", matches("^social_desirability_[A-I]-quantised$"), matches("^populism_[A-D]$"), starts_with("political_"))-> pp_lang_pop

pp_lang_pop%>%
  select( -`political_orientation_A-quantised`, -`political_orientation_B-quantised`, -`political_orientation_C-quantised`, -`political_orientation_D-quantised`, -`political_spectrum_other-quantised`)%>%
  pivot_longer(cols = 2:23, names_to = "variety_numeric", values_to = "lang.variety")%>%
  drop_na(lang.variety)-> pp_lang_pop

### Filter responses to the open text field regarding the languages of the caregivers
pp_lang_pop %>%
  distinct() -> pp_lang_pop #remove duplicates (result of merging two variables showing the languages of two caregivers into one)

pp_lang_pop %>%
  mutate(`languages_caregiver1-text_cap` = str_to_lower(`languages_caregiver1-text`),
         `languages_caregiver2-text_cap` = str_to_lower(`languages_caregiver2-text`)) -> pp_lang_pop

#### merge two variables into one
pp_lang_pop %>%
pivot_longer(cols = c(`languages_caregiver1-text_cap`, `languages_caregiver2-text_cap`), names_to = "col_names", values_to = "languages_caregivers-text") %>%
  drop_na(`languages_caregivers-text`) %>%
  select(-col_names) -> pp_lang_pop

#### clean up data frame so that the values of languages_caregivers-text only appears in the rows where lang.variety == "Anderer Dialekt"
pp_lang_pop %>%
  mutate(`languages_caregivers-text` = case_when(
    lang.variety != "Anderer Dialekt" ~ NA_character_,
    TRUE ~ `languages_caregivers-text`)) -> pp_lang_pop

#### filter  
pp_lang_pop %>%
  separate_rows(`languages_caregivers-text`, sep = ",") %>% 
  separate_rows(`languages_caregivers-text`, sep = "/") %>%
  separate_rows(`languages_caregivers-text`, sep = "\\s*\\bund\\b\\s*") %>%
  mutate(`languages_caregivers-text` = str_trim(`languages_caregivers-text`)) -> pp_lang_pop

pp_lang_pop %>%
  mutate(`languages_caregivers-text` = str_to_title(`languages_caregivers-text`)) -> pp_lang_pop

#### homogenize typos for subcategories
pp_lang_pop %>%
  mutate(lang.caregivers_sub = case_when(
    `languages_caregivers-text` %in% c("Berlin", "Berliner", "Berliner Deutsch", "Berliner Dialekt", "Berlinerisch") ~ "Berlinerisch",
    `languages_caregivers-text` %in% c("Ruhrgebiet", "Ruhrpott", "Ruhrpottslang", "Ruhrdeutsch") ~ "Ruhrdeutsch",
    `languages_caregivers-text` %in% c("Brandenburg", "Brandenburgisch") ~ "Brandenburgisch",
    `languages_caregivers-text` %in% c("Fränkisch", "Plattdeutsch", "Bayrisch") ~ NA_character_,
    TRUE ~ `languages_caregivers-text`
  )) -> pp_lang_pop

#### sort answers into existing categories
pp_lang_pop %>%
  mutate(lang.caregivers_hom = case_when(
    `languages_caregivers-text` %in% c("Bayrisch", "Münchnerisch") ~ "Bairisch",
    lang.caregivers_sub == "Berlinerisch" ~ "Ostdeutsch",
    `languages_caregivers-text` %in% c("Russisch", "Ungarisch", "Venezianisch", "Polnisch") ~ "Dialekt/Sprache aus dem nicht-deutschsprachigen Ausland",
    `languages_caregivers-text` %in% c("Ruhrgebiet", "Ruhrpott", "Ruhrpottslang", "Ruhrdeutsch") ~ "Ruhrdeutsch",
    `languages_caregivers-text` %in% c("Mannheimerisch", "Oberpfälzerisch", "Kurpfälzisch") ~ "Pfälzisch",
    `languages_caregivers-text` == "Rheinhessisch" ~ "Hessisch",
    `languages_caregivers-text` %in% c("Niederösterreichisch", "Wienerisch") ~ "Österreichisch",
    `languages_caregivers-text` %in% c("Schlesisch", "Oberlausitzer Dialekt", "Vogtlandisch (Fränkisch)", "Niederlausitzer Mundart", "Brandenburg", "Brandenburgisch", "Erzgebirgisch") ~ "Ostdeutsch",
    `languages_caregivers-text` %in% c("Siegerländer Plattdeutsch", "Eifeler Deutsch") ~ "Moselfränkisch",
    `languages_caregivers-text` == "Kölsch" ~ "Rheinisch",
    `languages_caregivers-text` == "Fränkisch" ~ "Fränkisch",
    `languages_caregivers-text` == "Plattdeutsch" ~ "Plattdeutsch"
    )) -> pp_lang_pop

pp_lang_pop %>%
  mutate(lang.variety = case_when(
    lang.variety == "Anderer Dialekt" & lang.caregivers_hom %in% c("Hochdeutsch", "Alemannisch", "Badisch", "Bairisch", "Fränkisch", "Schweizerdeutsch", "Hessisch", "Moselfränkisch", "Norddeutsch", "Österreichisch", "Ostdeutsch", "Pfälzisch", "Rheinisch", "Saarländisch", "Sächsisch", "Schwäbisch", "Thüringisch", "Plattdeutsch", "Dialekt/Sprache aus dem nicht-deutschsprachigen Ausland", "Ich weiß nicht", "Anderer Dialekt") ~ lang.caregivers_hom,
    TRUE ~ lang.variety)) -> pp_lang_pop

pp_lang_pop %>%
  select(-`languages_caregiver1-text`, -`languages_caregiver2-text`) -> pp_lang_pop


pp_lang_pop%>%
  select( -`political_orientation_A-quantised`, -`political_orientation_B-quantised`, -`political_orientation_C-quantised`, -`political_orientation_D-quantised`, -`political_spectrum_other-quantised`)->pp_lang_pop

### turn the two variables that give info about the caregivers' languages into one
pp_lang_pop %>%
  pivot_longer(cols = starts_with("languages_caregiver1") & !contains("languages_caregiver1-text"), names_to = "variety_numeric1", values_to = "lang.variety1") %>%
  pivot_longer(cols = starts_with("languages_caregiver2") & !contains("languages_caregiver2-text"), names_to = "variety_numeric2", values_to = "lang.variety2") -> pp_lang_pop

pp_lang_pop%>%
  drop_na(lang.variety1, lang.variety2)%>%
  select (-`variety_numeric1`, -`variety_numeric2`)->pp_lang_pop

pp_lang_pop %>%
  pivot_longer(cols = c(lang.variety1, lang.variety2), names_to = "col_names", values_to = "lang.variety") %>%
  select(-col_names) -> pp_lang_pop


## summary tables for pp lang. & political bckgr

pp_lang_pop%>%
  group_by (`Participant Public ID`)%>%
  count(`Participant Public ID`)-> pp_lang_pop_rows # 1-7 rows per pp

pp_lang_pop%>%
  add_count(`Participant Public ID`)%>%
  filter(n > 1)%>%
  distinct()-> duplicates


pp_lang_pop%>%
  ungroup()%>%
  summarise(n_distinct(`Participant Public ID`))#1313

pp_lang_pop%>%
  group_by(party)%>%
  summarise(n_distinct(`Participant Public ID`))->sum_party

pp_lang_pop%>%
  group_by(lang.variety)%>%
  summarise(n_distinct(`Participant Public ID`))->sum_lang.variety_caregiver

pp_lang_pop%>%
  summarise(mean(own_dialect), sd (own_dialect), min (own_dialect), max(own_dialect))->own_lang.variety


## join to questionnaire data sets: 
pp_lang_pop%>%
left_join (pp_bckgr, by= c("Participant Public ID" = "Participant Public ID"))-> combined_questionnaire


#write_delim(questionnaire_final, here("data_processed", "questionnaires.csv"), col_names = TRUE, delim = ",")

# Write final data set

#write_delim(questionnaire_final, here("data_processed", "data_clean.csv"), col_names = TRUE, delim = ",")