# Hair products and DNAm (data cleaning and wrangling)


# Load packages -----------
library(tidyverse)
library(ggplot2)
library(skimr) # data summary
library(table1)
library(haven)
library(jstable)
library(survey)
library(tableone)
library(xlsx)

# Load dataset ------------
data <- read_sas("raw_data/dr00287_08_01.sas7bdat")


# Exposure variables ------------
data1 <- data %>%
  mutate(P129 = case_when(P128==0 ~ 1, P128==1 & is.na(P129) ~ 2, P128==1 & !is.na(P129) ~ P129, is.na(P128) ~ P129, TRUE ~ P129), 
         P134 = case_when(P133==0 ~ 1, P133==1 & is.na(P134) ~ 2, P133==1 & !is.na(P134) ~ P134, is.na(P133) ~ P134, TRUE ~ P134), 
         P121 = case_when(P120==0 ~ 1, P120==1 & is.na(P121) ~ 2, P120==1 & !is.na(P121) ~ P121, is.na(P120) ~ P121, TRUE ~ P121), 
         P126 = case_when(P125==0 ~ 1, P125==1 & is.na(P126) ~ 2, P125==1 & !is.na(P126) ~ P126, is.na(P125) ~ P126, TRUE ~ P126), 
         P118 = case_when(P117==0 ~ 1, P117==1 & is.na(P118) ~ 2, P117==1 & !is.na(P118) ~ P118, is.na(P117) ~ P118, TRUE ~ P118), 
         P137 = case_when(P136==0 ~ 1, P136==1 & is.na(P137) ~ 2, P136==1 & !is.na(P137) ~ P137, is.na(P136) ~ P137, TRUE ~ P137), 
         P140 = case_when(P139==0 ~ 1, P139==1 & is.na(P140) ~ 2, P139==1 & !is.na(P140) ~ P140, is.na(P139) ~ P140, TRUE ~ P140), 
         P143 = case_when(P142==0 ~ 1, P142==1 & is.na(P143) ~ 2, P142==1 & !is.na(P143) ~ P143, is.na(P142) ~ P143, TRUE ~ P143), 
         P146 = case_when(P145==0 ~ 1, P145==1 & is.na(P146) ~ 2, P145==1 & !is.na(P146) ~ P146, is.na(P145) ~ P146, TRUE ~ P146), 
         P149 = case_when(P148==0 ~ 1, P148==1 & is.na(P149) ~ 2, P148==1 & !is.na(P149) ~ P149, is.na(P148) ~ P149, TRUE ~ P149))

mutate.var <- function(x)(ifelse(x == 1, 0, # did not use 
                                 ifelse(x %in% c(2,3), 1, # < 1-2 times a year; every 3-4 months; # less than once a month and 1-3 times per month
                                        ifelse(x %in% c(4,5,6),2, NA)))) # every 5-8 weeks; once a month; more than once a month # 1-5 times per weeks and above
mutate.yn <- function(x)(ifelse(x == 1, 0, # did not use
                                ifelse(x %in% c(2,3,4,5,6), 1, NA))) # ever

# Product use
product <- c("PC85","PC89","PC91","PC95","PC97","PC99","PC101","PC103","PC105","PC107", # Main study
            "P129","P134","P121","P126","P118","P137","P140","P143","P146","P149")  # Vanguard
product_name <- c("exp_PermDye", "exp_PermDye_other", "exp_SemiPermDye", "exp_SemiPermDye_other", "exp_TempDye", 
             "exp_Bleach", "exp_Highlight", "exp_Straightener", "exp_Straightener_other", "exp_Perm")

# Combine main and vanguard 
data2 <- data1 %>% 
  mutate(exp_PermDye = ifelse(is.na(PC85), P129, PC85),
         exp_PermDye_other = ifelse(is.na(PC89), P134, PC89),
         exp_SemiPermDye = ifelse(is.na(PC91), P121, PC91),
         exp_SemiPermDye_other = ifelse(is.na(PC95), P126, PC95),
         exp_TempDye = ifelse(is.na(PC97), P118, PC97),
         exp_Bleach = ifelse(is.na(PC99), P137, PC99),
         exp_Highlight = ifelse(is.na(PC101), P140, PC101),
         exp_Straightener = ifelse(is.na(PC103), P143, PC103), 
         exp_Straightener_other = ifelse(is.na(PC105), P146, PC105),
         exp_Perm = ifelse(is.na(PC107), P149, PC107),
         
         exp_PermDye_duration = PC87, 
         exp_SemiPermDye_duration = PC93,
         
         exp_PermDye_dark = PC86dark, 
         exp_PermDye_light = PC86light, 
         exp_SemiPermDye_dark = PC92dark, 
         exp_SemiPermDye_light = PC92light) %>%
  
  mutate(exp_Perm_cat3_2 = case_when(exp_Perm==1~0,
                                    exp_Perm==2~1,
                                    exp_Perm%in%c(3,4,5,6)~2)) 


# Create exposure categories 
data3 <- tibble(data2) %>%
  mutate_at(all_of(product_name), funs(yn=mutate.yn(.)))%>% 
  mutate_at(all_of(product_name), funs(cat3=mutate.var(.)))%>%
  
  mutate(exp_PermDye_duration = ifelse(exp_PermDye_yn==0, 1, exp_PermDye_duration))%>%
  mutate(exp_SemiPermDye_duration = ifelse(exp_SemiPermDye_duration==0, 1, exp_SemiPermDye_duration))%>%
  mutate(exp_PermDye_dark = replace(exp_PermDye_dark, exp_PermDye_yn == 0, 0))%>%
  mutate(exp_PermDye_light = replace(exp_PermDye_light, exp_PermDye_yn == 0, 0))%>%
  mutate(exp_SemiPermDye_dark = replace(exp_SemiPermDye_dark, exp_SemiPermDye_yn == 0, 0))%>%
  mutate(exp_SemiPermDye_light = replace(exp_SemiPermDye_light, exp_SemiPermDye_yn == 0, 0))


# Covariates -------------------------
data4 <- data3 %>%
  mutate(age = AgeExact_Baseline, 
         
         educ = case_when(SE18 %in% c(1,2,3,4,5) ~ 0,  # HS or less 
                          SE18 %in% c(6,7) ~ 1,    # some college
                          SE18 %in% c(8,9,10) ~ 2),  # college or higher 

         
         BMIcat1 = case_when(EX_BMI_CDC_final %in% c(1,2) ~ 0, 
                             EX_BMI_CDC_final == 3 ~ 1, 
                             EX_BMI_CDC_final %in% c(4,5,6) ~ 2),
         
         BMIcat2 = case_when(EX_BMI_CDC_final == 1 ~ 0,
                             EX_BMI_CDC_final == 2 ~ 1,
                             EX_BMI_CDC_final == 3 ~ 2, 
                             EX_BMI_CDC_final %in% c(4,5,6) ~ 3), 
         
         BMI = EX_BMI_final,
         
         alcohol = case_when(AL_DrinkCat6 %in% c(0,1) ~ 0, # never and past 
                             AL_DrinkCat6 == 2 ~ 1, # current <1 drink  
                             AL_DrinkCat6 %in% c(3,4,5) ~ 2), # current >=1 drinks 
         
         race = case_when (SE_RACE_ETH == 0 ~ 0, # non-Hispanic white
                            SE_RACE15 %in% c(3,4,11,12) ~ 1, # all Black, 
                            SE_RACE15 %in% c(2,6,8,10,14) ~ 2,
                            !is.na(SE_RACE15) ~ 3),
         
         PA = PH_CurrentTotMETHrsPerWeek, 
         
         smoking = SM_SmokeStatusN, # never, past or current
         
         menopause = HR_Menopause_T0, 

         income = case_when(SE19Impute %in% c(1,2) ~ 0, # <50,000
                            SE19Impute %in% c(3) ~ 1,   # 50,000-<100,000
                            SE19Impute %in% c(4,5) ~ 2 )) # >=100,000

# Outcome -----------------------
# "_4" used 450k for the overlapped samples 
# "_8" used 850K for the overlapped samples
data4 <- data4 %>%
  mutate(Levine_aa_4 =  ifelse(is.na(Meth450K_PhenoAge_aa), Meth850K_BL_PhenoAge_aa, Meth450K_PhenoAge_aa), 
         Levine_aa_8 =  ifelse(is.na(Meth850K_BL_PhenoAge_aa), Meth450K_PhenoAge_aa, Meth850K_BL_PhenoAge_aa), 
         
         Grim_aa_4 =  ifelse(is.na(Meth450K_GrimAge_aa), Meth850K_BL_GrimAge_aa, Meth450K_GrimAge_aa), 
         Grim_aa_8 =  ifelse(is.na(Meth850K_BL_GrimAge_aa), Meth450K_GrimAge_aa, Meth850K_BL_GrimAge_aa), 
         
         PACE_4 = ifelse(is.na(Meth450K_DunedinPACE), Meth850K_BL_DunedinPACE, Meth450K_DunedinPACE), 
         PACE_8 = ifelse(is.na(Meth850K_BL_DunedinPACE), Meth450K_DunedinPACE, Meth850K_BL_DunedinPACE), 
         
         Ind_4 = ifelse(is.na(Meth450K_Levine_DNAm_age), "850K", "450K"), # indicator variables for what array was used
         Ind_8 = ifelse(is.na(Meth850K_BL_Levine_DNAm_age), "450K", "850K"))

data5 <- data4 %>%
  filter(!is.na(Levine_aa_4)) %>% #4482
  filter_at(vars(exp_PermDye_yn, exp_SemiPermDye_yn, exp_Straightener_yn, exp_Perm_yn), any_vars(!is.na(.))) %>% #4403
  filter_at(vars(age, race, educ, income, menopause, BMIcat1, PA, smoking, alcohol), all_vars(!is.na(.))) #4359


write_csv(data5, "clean_data/DNAm.csv")


# Weight ------------------------
skim(data5$MethPooled_CaseNoncase_Weight)

# Table 1 -----------------------
tableone <- data5 %>%
  select(age, race, educ, income,  
         BMIcat1, BMIcat2, PA, menopause,
         smoking, alcohol, 
         Levine_aa_8, Grim_aa_8, PACE_8, 
         exp_PermDye_cat3, exp_SemiPermDye_cat3, exp_Straightener_cat3, exp_Perm_cat3_2) %>%
  mutate_at(vars(race, educ, income, BMIcat1, BMIcat2, smoking, alcohol,
                 exp_PermDye_cat3, exp_SemiPermDye_cat3, exp_Straightener_cat3, exp_Perm_cat3_2), as.factor) 

label(tableone$age) <- "Age (years)"

tableone$race <- factor(tableone$race, levels=c(0,1), labels=c("Non-Hispanic White", "Black")) 
label(tableone$race) <- 'Race and Ethnicity'

tableone$educ <- factor(tableone$educ, levels=c(0,1,2), labels = c("High school or less", "Some college", "College or above"))
label(tableone$educ) <- "Educational Attainment"

tableone$income <- factor(tableone$income, levels=c(0,1,2), labels = c("<50,000","50,000-<100,000", "100,000"))
label(tableone$income) <- "Household Annual Income"

tableone$BMIcat1 <- factor(tableone$BMIcat1, levels = c(0,1,2), labels = c("Underweight and Normal", "Overweight", "Obesity"))
label(tableone$BMIcat1) <- "Body Mass Index"

label(tableone$PA) <- "Total Average Metabolic Equivalent (MET) (hours/week)"

tableone$smoking <- factor(tableone$smoking, levels = c(0,1,2),labels = c("Never", "Past", "Current"))
label(tableone$smoking) <-"Smoking Status"

tableone$alcohol <- factor(tableone$alcohol, levels = c(0,1,2), labels = c("Never or past", "Current <1 drink/day", "Current >=1 drinks/day"))
label(tableone$alcohol) <-"Alcohol Consumption" 

tableone$menopause <- factor(tableone$menopause, levels=c(0,1), labels=c("Premenopause", "Postmenopause")) 
label(tableone$menopause) <- 'Menopause'


label(tableone$Levine_aa_8) <-"PhenoAge, age acceleration"
label(tableone$Grim_aa_8) <-"GrimAge, age acceleration"
label(tableone$PACE_8) <-"DunedinPACE"


tableone$exp_PermDye_cat3 <- factor(tableone$exp_PermDye_cat3, levels = c(0,1,2), labels = c("Never", "Less Frequent","Frequent"))
label(tableone$exp_PermDye_cat3) <-"Use of Permanent Dye"

tableone$exp_SemiPermDye_cat3 <- factor(tableone$exp_SemiPermDye_cat3, levels = c(0,1,2), labels = c("Never", "Less Frequent","Frequent"))
label(tableone$exp_SemiPermDye_cat3) <-"Use of Semi-Permanent Dye"

tableone$exp_Straightener_cat3 <- factor(tableone$exp_Straightener_cat3, levels = c(0,1,2),labels = c("Never", "Less Frequent","Frequent"))
label(tableone$exp_Straightener_cat3) <-"Use of Straighteners/Relaxers"

tableone$exp_Perm_cat3_2 <- factor(tableone$exp_Perm_cat3_2, levels = c(0,1,2),labels = c("Never", "Less Frequent","Frequent"))
label(tableone$exp_Perm_cat3_2) <-"Use of Hair Permanents/Body Waves"


(tab1 = table1(~.|race, data = tableone))



# Weighted table 1 -----
tableone <- data5 %>%
  select(age, race, educ, income,  
         BMIcat1, BMIcat2, PA, menopause,
         smoking, alcohol, 
         Levine_aa_8, Grim_aa_8, PACE_8, 
         exp_PermDye_cat3, exp_SemiPermDye_cat3, exp_Straightener_cat3, exp_Perm_cat3_2,MethPooled_CaseNoncase_Weight) %>%
  mutate_at(vars(race, educ, income, BMIcat1, BMIcat2, smoking, alcohol,
                 exp_PermDye_cat3, exp_SemiPermDye_cat3, exp_Straightener_cat3, exp_Perm_cat3_2), as.factor) 


dstrat = svydesign(ids = ~ 0, weights = ~ MethPooled_CaseNoncase_Weight, data = tableone)

tb1 = svyCreateTableOne2(vars=c("age", "race", "educ", "income","menopause", "BMIcat1", "BMIcat2", "smoking", "alcohol", "PA", "Levine_aa_8", "Grim_aa_8", "PACE_8", "exp_PermDye_cat3", 
                                "exp_SemiPermDye_cat3", "exp_Straightener_cat3", "exp_Perm_cat3_2"),  
                         factorVars=c("race","educ","income","menopause", "BMIcat1", "BMIcat2", "smoking", "alcohol", "exp_PermDye_cat3", "exp_SemiPermDye_cat3", "exp_Straightener_cat3", "exp_Perm_cat3_2"), 
                         strata = "race", data=dstrat, showAllLevels=TRUE, 
                         Labels=TRUE, printToggle=TRUE, contDigits=1, catDigits=1)


View(as.data.frame(tb1))


tb2 = svyCreateTableOne(vars=c("age", "race", "educ", "income","menopause", "BMIcat1", "BMIcat2", "smoking", "alcohol", "PA", "Levine_aa_8", "Grim_aa_8", "PACE_8", "exp_PermDye_cat3", 
                               "exp_SemiPermDye_cat3", "exp_Straightener_cat3", "exp_Perm_cat3_2"),  
                        factorVars=c("race","educ","income","menopause", "BMIcat1","BMIcat2", "smoking", "alcohol", "exp_PermDye_cat3", "exp_SemiPermDye_cat3", "exp_Straightener_cat3", "exp_Perm_cat3_2"), 
                        data=dstrat)

print(tb2, nonnormal = c("age","PA", "Levine_aa_8", "Grim_aa_8", "PACE_8"), contDigits = 3, catDigits = 2,
      pDigits = 4, smd = TRUE)


table_b <- data5 %>%
  select(age, race, educ, income,  
         BMIcat1, PA, menopause,
         smoking, alcohol, 
         Levine_aa_8, Grim_aa_8, PACE_8, 
         exp_PermDye_cat3, exp_SemiPermDye_cat3, exp_Straightener_cat3, exp_Perm_cat3_2,MethPooled_CaseNoncase_Weight) %>%
  mutate_at(vars(race, educ, income, BMIcat1, smoking, alcohol,
                 exp_PermDye_cat3, exp_SemiPermDye_cat3, exp_Straightener_cat3, exp_Perm_cat3_2), as.factor)%>%
  filter(race==1)


dstrat = svydesign(ids = ~ 0, weights = ~ MethPooled_CaseNoncase_Weight, data = table_b)
black = svyCreateTableOne(vars=c("age", "race", "educ", "income", "BMIcat1", "smoking", "alcohol", "PA", "Levine_aa_8", "Grim_aa_8", "PACE_8", "exp_PermDye_cat3", 
                               "exp_SemiPermDye_cat3", "exp_Straightener_cat3", "exp_Perm_cat3_2"),  
                        factorVars=c("race","educ","income","BMIcat1","smoking", "alcohol", "exp_PermDye_cat3", "exp_SemiPermDye_cat3", "exp_Straightener_cat3", "exp_Perm_cat3_2"), 
                        data=dstrat)
print(black, nonnormal = c("age","PA", "Levine_aa_8", "Grim_aa_8", "PACE_8"), contDigits = 3, catDigits = 2,
      pDigits = 4, smd = TRUE)

table_w <- data5 %>%
  select(age, race, educ, income,  
         BMIcat1, PA, menopause,
         smoking, alcohol, 
         Levine_aa_8, Grim_aa_8, PACE_8, 
         exp_PermDye_cat3, exp_SemiPermDye_cat3, exp_Straightener_cat3, exp_Perm_cat3_2,MethPooled_CaseNoncase_Weight) %>%
  mutate_at(vars(race, educ, income, BMIcat1, smoking, alcohol,
                 exp_PermDye_cat3, exp_SemiPermDye_cat3, exp_Straightener_cat3, exp_Perm_cat3_2), as.factor)%>%
  filter(race==0)


dstrat = svydesign(ids = ~ 0, weights = ~ MethPooled_CaseNoncase_Weight, data = table_w)
white = svyCreateTableOne(vars=c("age", "race", "educ", "income", "BMIcat1", "smoking", "alcohol", "PA", "Levine_aa_8", "Grim_aa_8", "PACE_8", "exp_PermDye_cat3", 
                                 "exp_SemiPermDye_cat3", "exp_Straightener_cat3", "exp_Perm_cat3_2"),  
                          factorVars=c("race","educ","income","BMIcat1","smoking", "alcohol", "exp_PermDye_cat3", "exp_SemiPermDye_cat3", "exp_Straightener_cat3", "exp_Perm_cat3_2"), 
                          data=dstrat)
print(white, nonnormal = c("age","PA", "Levine_aa_8", "Grim_aa_8", "PACE_8"), contDigits = 3, catDigits = 2,
      pDigits = 4, smd = TRUE)




