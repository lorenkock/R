library(haven)
library(tidyverse)
library(ggpubr)
library(gee)
library(geepack)
library(survey)
library(parameters)
library(labelled)
library(gtsummary)


load(file = "/Users/lkock/Desktop/UVM/PATH/PUF1_5/DS5001/36498-5001-Data.rda")
load(file = "/Users/lkock/Desktop/UVM/PATH/PUF1_5/DS5211/36498-5211-Data.rda")
load(file = "/Users/lkock/Desktop/UVM/PATH/PUF1_5/DS5221/36498-5221-Data.rda")

wave_5 <- da36498.5001
wave_5_wgt <- da36498.5211
wave_5_wgt2 <- da36498.5221

df1 <- full_join(wave_5, wave_5_wgt, by = "PERSONID") %>%
  full_join(., wave_5_wgt2, by = "PERSONID") %>%
  select(PERSONID, everything())



# SAMPLE: Women age 18-44 who are not pregnant
# OUTCOME: ANDS use
# IV: Smoking status
# COV: Educational attainment (< associates degree vs >= associates degree)

df1 <- df1 %>% 
  mutate(educ = case_when(R05R_A_AM0018_V2 == "(1) 1 = Less than high school" | 
                            R05R_A_AM0018_V2 == "(2) 2 = GED" | 
                            R05R_A_AM0018_V2 == "(3) 3 = High school graduate" ~ "Less than associates degree",
                            R05R_A_AM0018_V2 == "(4) 4 = Some college (no degree) or associates degree" |
                            R05R_A_AM0018_V2 == "(5) 5 = Bachelor's degree or advanced degree" ~ "Associates degree or higher"))

table(df1$R05_AX0134_12M)

# Subset to 18-44 year old, female, not currently pregnant
df2 <- df1 %>% subset(R05R_A_AGECAT6 == "(1) 1 = 18 to 24 years old" |
                        R05R_A_AGECAT6 == "(2) 2 = 25 to 34 years old" |
                        R05R_A_AGECAT6 == "(3) 3 = 35 to 44 years old") %>%
  subset(R05R_A_SEX == "(2) 2 = Female") %>%
  subset(R05_AX0134_12M == "(2) 2 = No")

table(df2$R05R_A_CUR_ESTD_CIGS)


# Smoking status
# R05R_A_CUR_ESTD_CIGS

# Current ANDS use
# R05R_A_CUR_ESTD_EPRODS = Current established electronic nicotine product user
# R05R_A_CUR_ESTD_SNUS = Current established snus user
# R05R_A_CUR_ESTD_SMKLS = Current established smokeless tobacco user

df2 <- df2 %>% 
  mutate(ands_current = case_when(R05R_A_CUR_ESTD_EPRODS == "(1) 1 = Yes" |
                     R05R_A_CUR_ESTD_SNUS == "(1) 1 = Yes" ~ "Current establised ANDS use",
                   R05R_A_CUR_ESTD_EPRODS == "(2) 2 = No" |
                     R05R_A_CUR_ESTD_SNUS == "(2) 2 = No" ~ "NOT current establised ANDS use"))

table(df2$ands_current)
attributes(df2$R05R_A_EVR_EPRODS)

# Ever ANDS use
# R05R_A_EVR_EPRODS = Ever electronic nicotine product user
# R05R_A_EVR_SNUS = Ever snus user
# R05R_A_EVR_SMKLS = Ever smokeless tobacco user
# R05R_A_EVR_DISSBL = Ever dissolvable tobacco user

df2 <- df2 %>% 
  mutate(ands_ever = case_when(R05R_A_EVR_EPRODS == "(1) 1 = Yes" |
                                    R05R_A_EVR_SNUS == "(1) 1 = Yes" ~ "Ever ANDS use",
                                  R05R_A_EVR_EPRODS == "(2) 2 = No" |
                                    R05R_A_EVR_SNUS == "(2) 2 = No" ~ "Never ANDS use"))

table(df2$ands_ever)

# WEIGHTS
# Wave 5, Wave 1 cohort: R05_A_S01WGT
# Wave 5, Wave 1 cohort replicate weights: R05_A_S01WGT1 – R05_A_S01WGT100
# Wave 5, Wave 4 cohort: R05_A_S04WGT
# Wave 5, Wave 4 cohort replicate weights: R05_A_S04WGT1 – R05_A_S01WGT100

nrow(df2)

sum(is.na(df2$R05_A_S04WGT3))

df3 <- df2 %>%
  drop_na("R05_A_S04WGT")

nrow(df3)
View(df3)

df3 <- df3 %>%
  mutate(smoker = (case_when(R05R_A_CUR_ESTD_CIGS == "(2) 2 = No" ~ 0,
                                      R05R_A_CUR_ESTD_CIGS == "(1) 1 = Yes" ~ 1)))

df3 <- df3 %>%
  mutate(ands_current_2 = (case_when(ands_current == "NOT current establised ANDS use" ~ 0,
                                     ands_current == "Current establised ANDS use" ~ 1)))
# Factor
df3$smoker <- as.factor(df3$smoker)


table(df3$ands_current_2)

# Survey design
options(survey.replicates.mse = TRUE)

df3_svy <- svrepdesign(id = ~PERSONID,
                       weights = ~R05_A_S04WGT,
                       repweights = "R05_A_S04WGT[1-9]+",
                       type = "Fay",
                       rho = 0.3,
                       data = df3)


#### ANY ANDS USE (CURRENT) ####

addmargins(table(df3$ands_current))

# Overall 
svymean(~ands_current, design=df3_svy, na.rm = T)
confint(svymean(~ands_current, design=df3_svy, na.rm = T))

# By smoking status
View(svyby(~ands_current, ~smoker, svymean, design = df3_svy, na.rm = T))
View(confint(svyby(~ands_current, ~R05R_A_CUR_ESTD_CIGS, svymean, design = df3_svy, na.rm = T)))


svyglm(ands_current_2 ~ smoker + educ + smoker:educ, design = df3_svy)
summary(svyglm(ands_current_2 ~ smoker + educ + smoker:educ, design = df3_svy))

## By educational attainment
df3_svy_low <- subset(df3_svy, educ == "Less than associates degree")
df3_svy_high <- subset(df3_svy, educ == "Associates degree or higher")

# LESS EDUCATION
# Overall 
svymean(~ands_current, design=df3_svy_low, na.rm = T)
confint(svymean(~ands_current, design=df3_svy_low, na.rm = T))

# By smoking status
View(svyby(~ands_current, ~R05R_A_CUR_ESTD_CIGS, svymean, design = df3_svy_low, na.rm = T))
View(confint(svyby(~ands_current, ~R05R_A_CUR_ESTD_CIGS, svymean, design = df3_svy_low, na.rm = T)))

# MORE EDUCATION
# Overall 
svymean(~ands_current, design=df3_svy_high, na.rm = T)
confint(svymean(~ands_current, design=df3_svy_high, na.rm = T))

# By smoking status
View(svyby(~ands_current, ~R05R_A_CUR_ESTD_CIGS, svymean, design = df3_svy_high, na.rm = T))
View(confint(svyby(~ands_current, ~R05R_A_CUR_ESTD_CIGS, svymean, design = df3_svy_high, na.rm = T)))



#### ANY ANDS USE (EVER) ####

# Overall 
svymean(~ands_ever, design=df3_svy, na.rm = T)
confint(svymean(~ands_ever, design=df3_svy, na.rm = T))

# By smoking status
View(svyby(~ands_ever, ~R05R_A_CUR_ESTD_CIGS, svymean, design = df3_svy, na.rm = T))
View(confint(svyby(~ands_ever, ~R05R_A_CUR_ESTD_CIGS, svymean, design = df3_svy, na.rm = T)))

# LESS EDUCATION
# Overall 
svymean(~ands_ever, design=df3_svy_low, na.rm = T)
confint(svymean(~ands_ever, design=df3_svy_low, na.rm = T))

# By smoking status
View(svyby(~ands_ever, ~R05R_A_CUR_ESTD_CIGS, svymean, design = df3_svy_low, na.rm = T))
View(confint(svyby(~ands_ever, ~R05R_A_CUR_ESTD_CIGS, svymean, design = df3_svy_low, na.rm = T)))

# MORE EDUCATION
# Overall 
svymean(~ands_ever, design=df3_svy_high, na.rm = T)
confint(svymean(~ands_ever, design=df3_svy_high, na.rm = T))

# By smoking status
View(svyby(~ands_ever, ~R05R_A_CUR_ESTD_CIGS, svymean, design = df3_svy_high, na.rm = T))
View(confint(svyby(~ands_ever, ~R05R_A_CUR_ESTD_CIGS, svymean, design = df3_svy_high, na.rm = T)))

