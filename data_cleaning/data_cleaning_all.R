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
options(scipen=999)

# Task data
## read task data
audio_eval_male <- read_delim(here("raw_data", "data_exp_86579-v59_task-ue28.csv"), col_names = TRUE, delim = ",")
audio_eval_female <- read_delim(here("raw_data", "data_exp_86579-v59_task-v697.csv"), col_names = TRUE, delim = ",")
audio_eval_male_vs_female <-read_delim(here("raw_data", "data_exp_86579-v59_task-mdix.csv"), col_names = TRUE, delim = ",")
origin_authen_male <-read_delim(here("raw_data", "data_exp_86579-v59_task-brwb.csv"), col_names = TRUE, delim = ",")
origin_authen_female <-read_delim(here("raw_data", "data_exp_86579-v59_task-8u1p.csv"), col_names = TRUE, delim = ",")
origin_authen_male_vs_female <-read_delim(here("raw_data", "data_exp_86579-v59_task-r61o.csv"), col_names = TRUE, delim = ",")


# clean tasks male only
## Audio evaluation
## select relevant columns for audio evaluations & rename
audio_eval_male%>%
  select(`Participant Public ID`, `Response Type`, `Response`, `Object Name`, `Spreadsheet: Audio`, `Spreadsheet: Speaker`, `Spreadsheet: Speaker gender`, `Spreadsheet: Variety`,
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

## clean origin_authen_male & elaborate Origin & authenticity column

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

# duplicates & data loss in audio_eval_male?
 
 audio_eval_male%>%
   group_by(`Participant Public ID`)%>%
   count()-> pp_rows # 3 pp not normal row no of 168
 
 audio_eval_male%>%
   filter (`Participant Public ID` == "281093394820202" | `Participant Public ID` == "281348585989177" | `Participant Public ID` =="281427437681691")-> pp_audio_check

## Combine 3 data sets: questionnaire, audio eval, origin & authenticity
# bind audio evaluation with origin & authenticity questions
# left_join(audio_eval_male, origin_authen_male, by= "Participant Public ID")->Test_male # doesn't work

 # Perform the join
audio_eval_male %>%
   left_join(origin_authen_male, 
             by = c("Participant Public ID" = "Participant Public ID", 
                    "Audio_Track" = "Audio"))-> combined_data
 

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
  select(-starts_with("Spreadsheet:"), -`Response Type`)%>%
  mutate(Speaker_Gender = ifelse(Speaker_Gender=="FALSE", "female", "male"))%>%
  mutate (Polit_ori = ifelse(Polit_ori == "l", "left", "right"))%>%
  mutate (Lang.Variety_Audio = ifelse(Lang.Variety_Audio == "st", "Standard German", "Regional Variety"))-> audio_eval_male_vs_female

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
  select ("Participant Public ID", "Participant Private ID", "Participant Status", "UTC Timestamp", "Participant OS", "Participant Browser", "Task Name", "counterbalance-nimi", "counterbalance-x3xi", "counterbalance-xq3l", "randomiser-rtb5", "age", "gender", "state_of_residence", "education_school", "education_profession1", "education_profession2", "income", "profession", "profession-text", starts_with("languages_caregiver1"), starts_with("languages-caregiver2"), starts_with("political_"), "other_languages", "own_dialect", "own_dialect-quantised", "party", "party-text", "party-quantised", matches("^social_desirability_[A-I]-quantised$"), matches("^populism_[A-D]$"))-> questionnaire

# prep: assign classes
as.factor(questionnaire$`Participant Private ID`)->questionnaire$`Participant Private ID`
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
  group_by (`Participant Private ID`)%>%
  summarise (n_distinct (`UTC Timestamp`))-> multi_part 


# separate questionnaire into 2 data sets


## data set: technical stuff & demographics

questionnaire%>%
  select("Participant Public ID", "Participant Private ID", "Participant Status", "Participant OS", "Participant Browser", "Task Name", "counterbalance-nimi", "counterbalance-x3xi", "counterbalance-xq3l", "randomiser-rtb5", "age", "gender", "state_of_residence", "education_school", "education_profession1", "education_profession2", "income", "profession", "profession-text")->pp_bckgr

## check specific IVs language & populism

questionnaire%>%
  select("Participant Private ID", starts_with("languages_caregiver1"), starts_with("languages_caregiver2"), "other_languages", "own_dialect", "party", "party-text", matches("^social_desirability_[A-I]-quantised$"), matches("^populism_[A-D]$"), starts_with("political_"))-> pp_lang_pop



## create new dummy variables education

#### new variable: education & training


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



# create summary tables:


## summary to check randomization:

pp_bckgr%>%
  group_by(`randomiser-rtb5`,`counterbalance-nimi`, `counterbalance-x3xi`, `counterbalance-xq3l`)%>%
  summarise(n_distinct(`Participant Private ID`))->random


## summary to check: participant background

### federal state

pp_bckgr%>%
  group_by(state_of_residence)%>%
  summarise(n_distinct(`Participant Private ID`))->pp_state

### participant age

pp_bckgr%>%
  ungroup()%>%
  drop_na(age)%>%
  summarise(mean(age), sd(age), min (age), max (age))->pp_age

### participant gender

pp_bckgr%>%
  group_by(gender)%>%
  summarise(n_distinct(`Participant Private ID`))->pp_gender

### sum_education_school
pp_bckgr%>%
  ungroup()%>%
  group_by(education_school_sum)%>%
  summarise(n_distinct(`Participant Private ID`))->sum_education

### education
pp_bckgr%>%
  group_by(education_school)%>%
  summarise(n_distinct(`Participant Private ID`))->education

### sum_profession
pp_bckgr%>%
  group_by(education_profession1_sum)%>%
  summarise(n_distinct(`Participant Private ID`))->sum_profession1


### income

pp_bckgr%>%
  group_by(income)%>%
  summarise(n_distinct(`Participant Private ID`))->income

### profession

pp_bckgr%>%
  group_by(profession)%>%
  summarise(n_distinct(`Participant Private ID`))->sum_profession


## summary to check: pp lang. & political bckgr


pp_lang_pop%>%
  select( -`political_orientation_A-quantised`, -`political_orientation_B-quantised`, -`political_orientation_C-quantised`, -`political_orientation_D-quantised`, -`political_spectrum_other-quantised`)->pp_lang_pop

pp_lang_pop%>%
  pivot_longer(cols = 2:23, names_to = "variety_numeric", values_to = "lang.variety")->pp_lang_pop

pp_lang_pop%>%
  drop_na(lang.variety)%>%
  select (-`variety_numeric`)->pp_lang_pop

pp_lang_pop%>%
  select(`Participant Private ID`, other_languages, own_dialect, lang.variety, everything())->pp_lang_pop

### political backgr

pp_lang_pop%>%
  group_by(party)%>%
  summarise(n_distinct(`Participant Private ID`))->sum_party

### lang. variety

pp_lang_pop%>%
  group_by(lang.variety)%>%
  summarise(n_distinct(`Participant Private ID`))->sum_lang.variety_caregiver

pp_lang_pop%>%
  summarise(mean(own_dialect), sd (own_dialect), min (own_dialect), max(own_dialect))->own_lang.variety


## unite two separate data sets for further analyses

#full_join(pp_lang_pop, pp_bckgr, by= "Participant Private ID")-> questionnaire_final

#write_delim(questionnaire_final, here("data_processed", "questionnaires.csv"), col_names = TRUE, delim = ",")

# Write final data set

#write_delim(questionnaire_final, here("data_processed", "data_clean.csv"), col_names = TRUE, delim = ",")