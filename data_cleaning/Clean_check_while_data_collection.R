# load libraries for cleaning #

library (dplyr)
library(broom)
library(tidyverse)
library(corrr)
library(ggcorrplot)
library(car)
library(readr)
library(here)
options(scipen=999)

# *read raw data*

questionnaire <- read_delim(here("raw_data", "data_exp_86579-v59_questionnaire-ry6q.csv"), col_names = TRUE, delim = ",")
as_tibble(questionnaire)
questionnaire

# check & get relevant columns'

colnames (questionnaire)

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


## check check technical stuff & representative of sample/ demographics

questionnaire%>%
  select("Participant Private ID", "Participant Status", "Participant OS", "Participant Browser", "Task Name", "counterbalance-nimi", "counterbalance-x3xi", "counterbalance-xq3l", "randomiser-rtb5", "age", "gender", "state_of_residence", "education_school", "education_profession1", "education_profession2", "income", "profession", "profession-text")->pp_bckgr

## check specific IVs language & populism

questionnaire%>%
  select("Participant Private ID", starts_with("languages_caregiver1"), starts_with("languages_caregiver2"), "other_languages", "own_dialect", "party", "party-text", matches("^social_desirability_[A-I]-quantised$"), matches("^populism_[A-D]$"), starts_with("political_"))-> pp_lang_pop



## create new dummy variables education

# new variable: education & training


 

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

colnames(pp_bckgr)

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


# pp lang. & political bckgr


pp_lang_pop%>%
  select( -`political_orientation_A-quantised`, -`political_orientation_B-quantised`, -`political_orientation_C-quantised`, -`political_orientation_D-quantised`, -`political_spectrum_other-quantised`)->pp_lang_pop

pp_lang_pop%>%
  pivot_longer(cols = 2:23, names_to = "variety_numeric", values_to = "lang.variety")->pp_lang_pop

pp_lang_pop%>%
  drop_na(lang.variety)%>%
  select (-`variety_numeric`)->pp_lang_pop

colnames(pp_lang_pop)

pp_lang_pop%>%
  select(`Participant Private ID`, other_languages, own_dialect, lang.variety, everything())->pp_lang_pop

# political backgr

pp_lang_pop%>%
  group_by(party)%>%
  summarise(n_distinct(`Participant Private ID`))->sum_party

# lang. variety

pp_lang_pop%>%
  group_by(lang.variety)%>%
summarise(n_distinct(`Participant Private ID`))->sum_lang.variety_caregiver

pp_lang_pop%>%
  summarise(mean(own_dialect), sd (own_dialect), min (own_dialect), max(own_dialect))->own_lang.variety


## unite two separate data sets for further analyses

#full_join(pp_lang_pop, pp_bckgr, by= "Participant Private ID")-> questionnaire_final

#write_delim(questionnaire_final, here("data_processed", "questionnaires.csv"), col_names = TRUE, delim = ",")