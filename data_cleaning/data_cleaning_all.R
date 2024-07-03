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

# read questionnaire data

questionnaire <- read_delim(here("data_processed", "questionnaire_final.csv"), col_names = TRUE, delim = ",")
as_tibble(questionnaire)
questionnaire

## read task data


audio_eval_male <- read_delim(here("raw_data", "data_exp_86579-v59_task-ue28.csv"), col_names = TRUE, delim = ",")
audio_eval_female <- read_delim(here("raw_data", "data_exp_86579-v59_task-v697.csv"), col_names = TRUE, delim = ",")
audio_eval_male_vs_female <-read_delim(here("raw_data", "data_exp_86579-v59_task-mdix.csv"), col_names = TRUE, delim = ",")
origin_authen_male <-read_delim(here("raw_data", "data_exp_86579-v59_task-brwb.csv"), col_names = TRUE, delim = ",")
origin_authen_female <-read_delim(here("raw_data", "data_exp_86579-v59_task-8u1p.csv"), col_names = TRUE, delim = ",")
origin_authen_male_vs_female <-read_delim(here("raw_data", "data_exp_86579-v59_task-r61o.csv"), col_names = TRUE, delim = ",")

# clean audio_eval_male

## select relevant columns for audio evaluations

audio_eval_male%>%
  select(`Participant Public ID`, `Participant Private ID`, `Response Type`, `Response`, `Object Name`, `Spreadsheet: Audio`, `Spreadsheet: Speaker`, `Spreadsheet: Speaker gender`, `Spreadsheet: Variety`,
         `Spreadsheet: Political orientation`, `Spreadsheet: Stimulus number`, `Spreadsheet: Stimulus1`, `Spreadsheet: Stimulus2`, `Spreadsheet: Stimulus3`, `Spreadsheet: Stimulus4`, `Spreadsheet: Stimulus5`, 
         `Spreadsheet: Stimulus6`, `Spreadsheet: Attention Check`)%>%
  filter(`Response Type` == "response")%>%
  filter (`Spreadsheet: Speaker` != "Practice_Spkr")->audio_eval_male

#rename columns

audio_eval_male%>%
  rename(Audio_Track = `Spreadsheet: Audio`, Speaker = `Spreadsheet: Speaker`, Speaker_Gender = `Spreadsheet: Speaker gender`, Lang.Variety_Audio = `Spreadsheet: Variety`,
         Polit_ori = `Spreadsheet: Political orientation`, Statement_no = `Spreadsheet: Stimulus number`) -> audio_eval_male

# remove markdown from values

audio_eval_male%>%
mutate(across(starts_with("Spreadsheet:"), ~ str_remove_all(., "\\*\\*")))-> audio_eval_male

  
# assign classes
as.factor(audio_eval_male$`Spreadsheet: Stimulus1`)-> audio_eval_male$`Spreadsheet: Stimulus1`

# create one column with attribute related to rating

audio_eval_male%>%
mutate(Attribute = case_when(
  `Object Name` == "RatingScale_Stimulus1" ~ `Spreadsheet: Stimulus1`,
  `Object Name` == "RatingScale_Stimulus2" ~ `Spreadsheet: Stimulus2`,
  `Object Name` == "RatingScale_Stimulus3" ~ `Spreadsheet: Stimulus3`,
  `Object Name` == "RatingScale_Stimulus4" ~ `Spreadsheet: Stimulus4`,
  `Object Name` == "RatingScale_Stimulus5" ~ `Spreadsheet: Stimulus5`,
  `Object Name` == "RatingScale_Stimulus6" ~ `Spreadsheet: Stimulus6`,
  `Object Name` == "RatingScale_AttentionCheck" ~ `Spreadsheet: Attention Check`))->audio_eval_male

# delete unnecessary columns & elaborate levels of categorical variables

audio_eval_male%>%
  filter (!str_starts(Speaker, "Filler"))%>%
  select(-starts_with("Spreadsheet:"), -`Response Type`)%>%
  mutate(Speaker_Gender = ifelse(Speaker_Gender=="f", "female", "male"))%>%
  mutate (Polit_ori = ifelse(Polit_ori == "l", "left", "right"))%>%
  mutate (Lang.Variety_Audio = ifelse(Lang.Variety_Audio == "st", "Standard German", "Regional Variety"))-> audio_eval_male

# clean audio eval female

## select relevant columns for audio evaluations
audio_eval_female%>%
  select(`Participant Public ID`, `Participant Private ID`, `Response Type`, `Response`, `Object Name`, `Spreadsheet: Audio`, `Spreadsheet: Speaker`, `Spreadsheet: Speaker gender`, `Spreadsheet: Variety`,
         `Spreadsheet: Political orientation`, `Spreadsheet: Stimulus number`, `Spreadsheet: Stimulus1`, `Spreadsheet: Stimulus2`, `Spreadsheet: Stimulus3`, `Spreadsheet: Stimulus4`, `Spreadsheet: Stimulus5`, 
         `Spreadsheet: Stimulus6`, `Spreadsheet: Attention Check`)%>%
  filter(`Response Type` == "response")%>%
  filter (`Spreadsheet: Speaker` != "Practice_Spkr")->audio_eval_female

## rename columns
audio_eval_female%>%
  rename(Audio_Track = `Spreadsheet: Audio`, Speaker = `Spreadsheet: Speaker`, Speaker_Gender = `Spreadsheet: Speaker gender`, Lang.Variety_Audio = `Spreadsheet: Variety`,
         Polit_ori = `Spreadsheet: Political orientation`, Statement_no = `Spreadsheet: Stimulus number`) -> audio_eval_female

## remove markdown from values
audio_eval_female%>%
  mutate(across(starts_with("Spreadsheet:"), ~ str_remove_all(., "\\*\\*")))-> audio_eval_female

## assign classes
as.factor(audio_eval_female$`Spreadsheet: Stimulus1`)-> audio_eval_female$`Spreadsheet: Stimulus1`

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
  filter (!str_starts(Speaker, "Filler"))%>%
  select(-starts_with("Spreadsheet:"))%>%
  select(-`Response Type`)%>%
  mutate(Speaker_Gender = ifelse(Speaker_Gender=="FALSE", "female", "male"))%>%
  mutate (Polit_ori = ifelse(Polit_ori == "l", "left", "right"))%>%
  mutate (Lang.Variety_Audio = ifelse(Lang.Variety_Audio == "st", "Standard German", "Regional Variety"))-> audio_eval_female




# clean audio eval male vs. female

## select relevant columns for audio evaluations
audio_eval_male_vs_female%>%
  select(`Participant Public ID`, `Participant Private ID`, `Response Type`, `Response`, `Object Name`, `Spreadsheet: Audio`, `Spreadsheet: Speaker`, `Spreadsheet: Speaker gender`, `Spreadsheet: Variety`,
         `Spreadsheet: Political orientation`, `Spreadsheet: Stimulus number`, `Spreadsheet: Stimulus1`, `Spreadsheet: Stimulus2`, `Spreadsheet: Stimulus3`, `Spreadsheet: Stimulus4`, `Spreadsheet: Stimulus5`, 
         `Spreadsheet: Stimulus6`, `Spreadsheet: Attention Check`)%>%
  filter(`Response Type` == "response")%>%
  filter (`Spreadsheet: Speaker` != "Practice_Spkr")->audio_eval_male_vs_female

## rename columns
audio_eval_male_vs_female%>%
  rename(Audio_Track = `Spreadsheet: Audio`, Speaker = `Spreadsheet: Speaker`, Speaker_Gender = `Spreadsheet: Speaker gender`, Lang.Variety_Audio = `Spreadsheet: Variety`,
         Polit_ori = `Spreadsheet: Political orientation`, Statement_no = `Spreadsheet: Stimulus number`) -> audio_eval_male_vs_female

## remove markdown from values
audio_eval_male_vs_female%>%
  mutate(across(starts_with("Spreadsheet:"), ~ str_remove_all(., "\\*\\*")))-> audio_eval_male_vs_female

## assign classes
as.factor(audio_eval_male_vs_female$`Spreadsheet: Stimulus1`)-> audio_eval_male_vs_female$`Spreadsheet: Stimulus1`

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
  filter (!str_starts(Speaker, "Filler"))%>%
  select(-starts_with("Spreadsheet:"))%>%
  select(-`Response Type`)%>%
  mutate(Speaker_Gender_TEST = ifelse(Speaker_Gender=="f", "female", "male"))%>%
  mutate (Polit_ori = ifelse(Polit_ori == "l", "left", "right"))%>%
  mutate (Lang.Variety_Audio = ifelse(Lang.Variety_Audio == "st", "Standard German", "Regional Variety"))-> audio_eval_male_vs_female




# unite task data sets with questionnaire


# full_join(audio_eval_male, audio_eval_female, audio_eval_male_vs_female, questionnaire, by= "Participant Private ID")-> questionnaire_final

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



# create summary tables:


## check randomization:

pp_bckgr%>%
  group_by(`randomiser-rtb5`,`counterbalance-nimi`, `counterbalance-x3xi`, `counterbalance-xq3l`)%>%
  summarise(n_distinct(`Participant Private ID`))->random


## check: participant background

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


### political backgr

pp_lang_pop%>%
  group_by(party)%>%
  summarise(n_distinct(`Participant Private ID`))->sum_party

# Write final data set

#write_delim(questionnaire_final, here("data_processed", "data_clean.csv"), col_names = TRUE, delim = ",")