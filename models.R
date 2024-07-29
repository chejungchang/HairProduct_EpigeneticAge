# Hair products and DNAm 


# Load packages -----------
library(tidyverse)
library(ggplot2)
library(skimr) # data summary
library(table1)
library(survey)
library(broom)
library(lmtest)

# Load dataset ------------
dat <- read_csv("clean_data/DNAm.csv")

# Set the value great and less than 4 times SD to missing 
dat1 <- dat %>%
  mutate(PACE_8 = ifelse(abs(scale(PACE_8))>4, NA, PACE_8),
         Grim_aa_8 = ifelse(abs(scale(Grim_aa_8))>4, NA, Grim_aa_8),
         Levine_aa_8 = ifelse(abs(scale(Levine_aa_8))>4, NA, Levine_aa_8))

dat %>% summarise(across(all_of(c("Levine_aa_8", "Grim_aa_8", "PACE_8")), ~sum(is.na(.))))
dat1 %>% summarise(across(all_of(c("Levine_aa_8", "Grim_aa_8", "PACE_8")), ~sum(is.na(.))))

# Ever - never ----
cov = c("race", "educ", "income", "menopause","BMIcat1", 
        "smoking", "alcohol", "PA", "Ind_8", "MethPooled_CaseNoncase_Weight", "age")
outcome = c("PACE_8", "Grim_aa_8", "Levine_aa_8")

dat_regression_yn <- dat1 %>% 
  select(matches(c("exp_PermDye_yn", "exp_SemiPermDye_yn", "exp_Straightener_yn", "exp_Perm_yn")), all_of(cov), all_of(outcome)) %>%
  pivot_longer(cols = starts_with("exp_"), names_to = "Exposure", values_to = "Exposure_Value") %>% 
  group_by(Exposure) %>% nest() %>% 
  mutate(
    PACE = map(data, ~svyglm(PACE_8 ~ factor(Exposure_Value) + age + factor(race) + factor(educ) + factor(income) + factor(menopause) + 
                               factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8), 
                             family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.))), 
    Grimage = map(data, ~svyglm(Grim_aa_8 ~ factor(Exposure_Value) + age + factor(race) + factor(educ) + factor(income) + factor(menopause) + 
                                  factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8),
                             family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.))),
    Phenoage = map(data, ~svyglm(Levine_aa_8 ~ factor(Exposure_Value) + age + factor(race) + factor(educ) + factor(income) + factor(menopause) + 
                                   factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8),
                                family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.)))) %>%
  pivot_longer(cols = c(PACE, Grimage, Phenoage), names_to = "Outcome", values_to = "Model") %>%
  mutate(tidied = map(Model, tidy, conf.int = TRUE), summary = map(Model, glance)) %>%
  unnest(c(tidied, summary)) %>%
  filter(term=="factor(Exposure_Value)1") %>%
  select(Exposure, Outcome, estimate, p.value, conf.low, conf.high, nobs) 

View(dat_regression_yn)


# Frequency ----
dat_regression_cat <- dat1 %>% 
  select(matches(c("exp_PermDye_cat3", "exp_SemiPermDye_cat3", "exp_Straightener_cat3", "exp_Perm_cat3_2")), all_of(cov), all_of(outcome)) %>%
  pivot_longer(cols = starts_with("exp_"), names_to = "Exposure", values_to = "Exposure_Value") %>% 
  group_by(Exposure) %>% nest() %>% 
  mutate(
    PACE = map(data, ~svyglm(PACE_8 ~ factor(Exposure_Value) + age + factor(race) + factor(educ) + factor(income) + menopause + 
                               factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8), 
                             family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.))), 
    Grimage = map(data, ~svyglm(Grim_aa_8 ~ factor(Exposure_Value) + age + factor(race) + factor(educ) + factor(income) + menopause + 
                                  factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8),
                                family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.))),
    Phenoage = map(data, ~svyglm(Levine_aa_8 ~ factor(Exposure_Value) + age + factor(race) + factor(educ) + factor(income) + menopause + 
                                   factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8),
                                 family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.)))) %>%
  pivot_longer(cols = c(PACE, Grimage, Phenoage), names_to = "Outcome", values_to = "Model") %>%
  mutate(tidied = map(Model, tidy, conf.int = TRUE), summary = map(Model, glance)) %>%
  unnest(c(tidied, summary)) %>%
  filter(term %in% c("factor(Exposure_Value)1", "factor(Exposure_Value)2")) %>%
  select(Exposure, Outcome, estimate, p.value, conf.low, conf.high, nobs) 

View(dat_regression_cat)


dat_regression_ptrend <- dat1 %>% 
  select(matches(c("exp_PermDye_cat3", "exp_SemiPermDye_cat3", "exp_Straightener_cat3", "exp_Perm_cat3_2")), all_of(cov), all_of(outcome)) %>%
  pivot_longer(cols = starts_with("exp_"), names_to = "Exposure", values_to = "Exposure_Value") %>% 
  group_by(Exposure) %>% nest() %>% 
  mutate(
    PACE = map(data, ~svyglm(PACE_8 ~ Exposure_Value + age + factor(race) + factor(educ) + factor(income) + menopause + 
                               factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8), 
                             family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.))), 
    Grimage = map(data, ~svyglm(Grim_aa_8 ~ Exposure_Value + age + factor(race) + factor(educ) + factor(income) + menopause + 
                                  factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8),
                                family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.))),
    Phenoage = map(data, ~svyglm(Levine_aa_8 ~ Exposure_Value + age + factor(race) + factor(educ) + factor(income) + menopause + 
                                   factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8),
                                 family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.)))) %>%
  pivot_longer(cols = c(PACE, Grimage, Phenoage), names_to = "Outcome", values_to = "Model") %>%
  mutate(tidied = map(Model, tidy, conf.int = TRUE), summary = map(Model, glance)) %>%
  unnest(c(tidied, summary)) %>%
  filter(term %in% c("Exposure_Value")) %>%
  select(Exposure, Outcome, estimate, p.value, conf.low, conf.high, nobs) 

View(dat_regression_ptrend)
dat_regression_ptrend %>% 
  mutate(p.value2 = round(p.value, 2)) 

# Race and ethnicity -----

cov = c("race", "educ", "income", "menopause","BMIcat1", "smoking", "alcohol", "PA", "Ind_8", "MethPooled_CaseNoncase_Weight", "age")
outcome = c("PACE_8", "Grim_aa_8", "Levine_aa_8")


dat_regression_yn_black <- dat1 %>% 
  select(matches(c("exp_PermDye_yn", "exp_SemiPermDye_yn", "exp_Straightener_yn", "exp_Perm_yn")), all_of(cov), all_of(outcome)) %>%
  pivot_longer(cols = starts_with("exp_"), names_to = "Exposure", values_to = "Exposure_Value") %>% 
  mutate(race = as.factor(race)) %>%
  group_by(Exposure) %>% nest() %>% 
  mutate(
    PACE = map(data, ~svyglm(PACE_8 ~ factor(Exposure_Value)*relevel(race,ref="1") + age + factor(educ) + factor(income) + menopause + 
                               factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8), 
                             family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.))), 
    Grimage = map(data, ~svyglm(Grim_aa_8 ~ factor(Exposure_Value)*relevel(race,ref="1") + age + factor(educ) + factor(income) + menopause + 
                                  factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8),
                                family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.))),
    Phenoage = map(data, ~svyglm(Levine_aa_8 ~ factor(Exposure_Value)*relevel(race,ref="1") + age + factor(educ) + factor(income) + menopause + 
                                   factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8),
                                 family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.)))) %>%
  pivot_longer(cols = c(PACE, Grimage, Phenoage), names_to = "Outcome", values_to = "Model") %>%
  mutate(tidied = map(Model, tidy, conf.int = TRUE), summary = map(Model, glance)) %>%
  unnest(c(tidied, summary)) %>%
  filter(term=="factor(Exposure_Value)1") %>%
  select(Exposure, Outcome, estimate, p.value, conf.low, conf.high, nobs) 
view(dat_regression_yn_black)


dat_regression_yn_white <- dat1 %>% 
  select(matches(c("exp_PermDye_yn", "exp_SemiPermDye_yn", "exp_Straightener_yn", "exp_Perm_yn")), all_of(cov), all_of(outcome)) %>%
  pivot_longer(cols = starts_with("exp_"), names_to = "Exposure", values_to = "Exposure_Value") %>% 
  mutate(race = as.factor(race)) %>%
  group_by(Exposure) %>% nest() %>% 
  mutate(
    PACE = map(data, ~svyglm(PACE_8 ~ factor(Exposure_Value)*relevel(race,ref="0") + age + factor(educ) + factor(income) + menopause + 
                               factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8), 
                             family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.))), 
    Grimage = map(data, ~svyglm(Grim_aa_8 ~ factor(Exposure_Value)*relevel(race,ref="0") + age + factor(educ) + factor(income) + menopause + 
                                  factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8),
                                family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.))),
    Phenoage = map(data, ~svyglm(Levine_aa_8 ~ factor(Exposure_Value)*relevel(race,ref="0") + age + factor(educ) + factor(income) + menopause + 
                                   factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8),
                                 family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.)))) %>%
  pivot_longer(cols = c(PACE, Grimage, Phenoage), names_to = "Outcome", values_to = "Model") %>%
  mutate(tidied = map(Model, tidy, conf.int = TRUE), summary = map(Model, glance)) %>%
  unnest(c(tidied, summary)) %>%
  filter(term=="factor(Exposure_Value)1") %>%
  select(Exposure, Outcome, estimate, p.value, conf.low, conf.high, nobs) 
view(dat_regression_yn_white)


dat_regression_yn_phet <- dat1 %>% 
  select(matches(c("exp_PermDye_yn", "exp_SemiPermDye_yn", "exp_Straightener_yn", "exp_Perm_yn")), all_of(cov), all_of(outcome)) %>%
  pivot_longer(cols = starts_with("exp_"), names_to = "Exposure", values_to = "Exposure_Value") %>% 
  mutate(race = as.factor(race)) %>%
  group_by(Exposure) %>% nest() %>% 
  mutate(
    PACE = map(data, ~svyglm(PACE_8 ~ factor(Exposure_Value)*relevel(race,ref="1") + age + factor(educ) + factor(income) + menopause + 
                               factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8), 
                             family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.))), 
    Grimage = map(data, ~svyglm(Grim_aa_8 ~ factor(Exposure_Value)*relevel(race,ref="1") + age + factor(educ) + factor(income) + menopause + 
                                  factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8),
                                family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.))),
    Phenoage = map(data, ~svyglm(Levine_aa_8 ~ factor(Exposure_Value)*relevel(race,ref="1") + age + factor(educ) + factor(income) + menopause + 
                                   factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8),
                                 family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.)))) %>%
  pivot_longer(cols = c(PACE, Grimage, Phenoage), names_to = "Outcome", values_to = "Model") %>%
  mutate(tidied = map(Model, tidy, conf.int = TRUE), summary = map(Model, glance)) %>%
  unnest(c(tidied, summary)) %>%
  filter(term=="factor(Exposure_Value)1:relevel(race, ref = \"1\")0") %>%
  select(Exposure, Outcome, estimate, p.value, conf.low, conf.high, nobs) 
view(dat_regression_yn_phet)


dat_regression_cat_black <- dat1 %>% 
  select(matches(c("exp_PermDye_cat3", "exp_SemiPermDye_cat3", "exp_Straightener_cat3", "exp_Perm_cat3_2")), all_of(cov), all_of(outcome)) %>%
  pivot_longer(cols = starts_with("exp_"), names_to = "Exposure", values_to = "Exposure_Value") %>% 
  mutate(race = as.factor(race)) %>%
  group_by(Exposure) %>% nest() %>% 
  mutate(
    PACE = map(data, ~svyglm(PACE_8 ~ factor(Exposure_Value)*relevel(race,ref="1") + age + factor(educ) + factor(income) + menopause + 
                               factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8), 
                             family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.))), 
    Grimage = map(data, ~svyglm(Grim_aa_8 ~ factor(Exposure_Value)*relevel(race,ref="1") + age + factor(educ) + factor(income) + menopause + 
                                  factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8),
                                family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.))),
    Phenoage = map(data, ~svyglm(Levine_aa_8 ~ factor(Exposure_Value)*relevel(race,ref="1") + age + factor(educ) + factor(income) + menopause + 
                                   factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8),
                                 family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.)))) %>% 
  pivot_longer(cols = c(PACE, Grimage, Phenoage), names_to = "Outcome", values_to = "Model") %>%
  mutate(tidied = map(Model, tidy, conf.int = TRUE), summary = map(Model, glance)) %>%
  unnest(c(tidied, summary)) %>%
  filter(term %in% c("factor(Exposure_Value)1", "factor(Exposure_Value)2")) %>%
  select(Exposure, Outcome, estimate, p.value, conf.low, conf.high, nobs) 
view(dat_regression_cat_black)

dat_regression_ptrend_black <- dat1 %>% 
  select(matches(c("exp_PermDye_cat3", "exp_SemiPermDye_cat3", "exp_Straightener_cat3", "exp_Perm_cat3_2")), all_of(cov), all_of(outcome)) %>%
  pivot_longer(cols = starts_with("exp_"), names_to = "Exposure", values_to = "Exposure_Value") %>% 
  mutate(race = as.factor(race)) %>%
  group_by(Exposure) %>% nest() %>% 
  mutate(
    PACE = map(data, ~svyglm(PACE_8 ~ Exposure_Value*relevel(race,ref="1") + age + factor(educ) + factor(income) + menopause + 
                               factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8), 
                             family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.))), 
    Grimage = map(data, ~svyglm(Grim_aa_8 ~ Exposure_Value*relevel(race,ref="1") + age + factor(educ) + factor(income) + menopause + 
                                  factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8),
                                family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.))),
    Phenoage = map(data, ~svyglm(Levine_aa_8 ~ Exposure_Value*relevel(race,ref="1") + age + factor(educ) + factor(income) + menopause + 
                                   factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8),
                                 family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.)))) %>% 
  pivot_longer(cols = c(PACE, Grimage, Phenoage), names_to = "Outcome", values_to = "Model") %>%
  mutate(tidied = map(Model, tidy, conf.int = TRUE), summary = map(Model, glance)) %>%
  unnest(c(tidied, summary)) %>%
  filter(term %in% c("Exposure_Value")) %>%
  select(Exposure, Outcome, estimate, p.value, conf.low, conf.high, nobs) 
view(dat_regression_ptrend_black)
dat_regression_ptrend_black %>% 
  mutate(p.value2 = round(p.value, 2)) 

dat_regression_cat_white <- dat1 %>% 
  select(matches(c("exp_PermDye_cat3", "exp_SemiPermDye_cat3", "exp_Straightener_cat3", "exp_Perm_cat3_2")), all_of(cov), all_of(outcome)) %>%
  pivot_longer(cols = starts_with("exp_"), names_to = "Exposure", values_to = "Exposure_Value") %>% 
  mutate(race = as.factor(race)) %>%
  group_by(Exposure) %>% nest() %>% 
  mutate(
    PACE = map(data, ~svyglm(PACE_8 ~ factor(Exposure_Value)*relevel(race,ref="0") + age + factor(educ) + factor(income) + menopause + 
                               factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8), 
                             family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.))), 
    Grimage = map(data, ~svyglm(Grim_aa_8 ~ factor(Exposure_Value)*relevel(race,ref="0") + age + factor(educ) + factor(income) + menopause + 
                                  factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8),
                                family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.))),
    Phenoage = map(data, ~svyglm(Levine_aa_8 ~ factor(Exposure_Value)*relevel(race,ref="0") + age + factor(educ) + factor(income) + menopause + 
                                   factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8),
                                 family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.)))) %>% 
  pivot_longer(cols = c(PACE, Grimage, Phenoage), names_to = "Outcome", values_to = "Model") %>%
  mutate(tidied = map(Model, tidy, conf.int = TRUE), summary = map(Model, glance)) %>%
  unnest(c(tidied, summary)) %>%
  filter(term %in% c("factor(Exposure_Value)1", "factor(Exposure_Value)2")) %>%
  select(Exposure, Outcome, estimate, p.value, conf.low, conf.high, nobs) 
view(dat_regression_cat_white)


dat_regression_ptrend_white <- dat1 %>% 
  select(matches(c("exp_PermDye_cat3", "exp_SemiPermDye_cat3", "exp_Straightener_cat3", "exp_Perm_cat3_2")), all_of(cov), all_of(outcome)) %>%
  pivot_longer(cols = starts_with("exp_"), names_to = "Exposure", values_to = "Exposure_Value") %>% 
  mutate(race = as.factor(race)) %>%
  group_by(Exposure) %>% nest() %>% 
  mutate(
    PACE = map(data, ~svyglm(PACE_8 ~ Exposure_Value*relevel(race,ref="0") + age + factor(educ) + factor(income) + menopause + 
                               factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8), 
                             family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.))), 
    Grimage = map(data, ~svyglm(Grim_aa_8 ~ Exposure_Value*relevel(race,ref="0") + age + factor(educ) + factor(income) + menopause + 
                                  factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8),
                                family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.))),
    Phenoage = map(data, ~svyglm(Levine_aa_8 ~ Exposure_Value*relevel(race,ref="0") + age + factor(educ) + factor(income) + menopause + 
                                   factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8),
                                 family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.)))) %>% 
  pivot_longer(cols = c(PACE, Grimage, Phenoage), names_to = "Outcome", values_to = "Model") %>%
  mutate(tidied = map(Model, tidy, conf.int = TRUE), summary = map(Model, glance)) %>%
  unnest(c(tidied, summary)) %>%
  filter(term %in% c("Exposure_Value")) %>%
  select(Exposure, Outcome, estimate, p.value, conf.low, conf.high, nobs) 
view(dat_regression_ptrend_white)
dat_regression_ptrend_white %>% 
  mutate(p.value2 = round(p.value, 2)) 



dat_regression_cat_phet <- dat1 %>% 
  select(matches(c("exp_PermDye_cat3", "exp_SemiPermDye_cat3", "exp_Straightener_cat3", "exp_Perm_cat3_2")), all_of(cov), all_of(outcome)) %>%
  pivot_longer(cols = starts_with("exp_"), names_to = "Exposure", values_to = "Exposure_Value") %>% 
  mutate(race = as.factor(race)) %>%
  group_by(Exposure) %>% nest() %>% 
  mutate(
    PACE = map(data, ~waldtest(svyglm(PACE_8 ~ factor(Exposure_Value)*relevel(race,ref="1") + age + factor(educ) + factor(income) + menopause + 
                                        factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8), 
                                      family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.)), 
                               svyglm(PACE_8 ~ factor(Exposure_Value)+relevel(race,ref="1") + age + factor(educ) + factor(income) + menopause + 
                                        factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8), 
                                      family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.)))), 
    Grimage = map(data, ~waldtest(svyglm(Grim_aa_8 ~ factor(Exposure_Value)*relevel(race,ref="1") + age + factor(educ) + factor(income) + menopause + 
                                        factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8), 
                                      family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.)), 
                               svyglm(Grim_aa_8 ~ factor(Exposure_Value)+relevel(race,ref="1") + age + factor(educ) + factor(income) + menopause + 
                                        factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8), 
                                      family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.)))),
    Phenoage = map(data, ~waldtest(svyglm(Levine_aa_8 ~ factor(Exposure_Value)*relevel(race,ref="1") + age + factor(educ) + factor(income) + menopause + 
                                        factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8), 
                                      family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.)), 
                               svyglm(Levine_aa_8 ~ factor(Exposure_Value)+relevel(race,ref="1") + age + factor(educ) + factor(income) + menopause + 
                                        factor(BMIcat1) + factor(smoking) + factor(alcohol) + PA + factor(Ind_8), 
                                      family = gaussian(), design = svydesign(ids=~0, weight=~MethPooled_CaseNoncase_Weight, data=.))))) %>%
  
  pivot_longer(cols = c(PACE, Grimage, Phenoage), names_to = "Outcome", values_to = "Model") %>%
  mutate(phet = map(Model, tidy, p.value)) %>%
  unnest(phet) %>%
  filter(df==-2) %>% filter((row_number() - 1) %% 3 == 0)%>%
  select(Exposure, Outcome, p.value) 
view(dat_regression_cat_phet)



