# Quit Aids

library(tidyverse)
library(broom)
library(survey)
library(ggthemes)
library(haven)
library(janitor)

# Load in data 
# sts <- read_sav("/Users/lkock/Desktop/Survey Data/STS/12_2024/Latest omnibus SPSS data file/omni218_39.1_65.2cot_31.3a_25.4s_recodes_104.5sa.sav")
# sts <- clean_names(sts)

# Filter data to those who made serious attempt
sts <- sts %>% 
  filter(xwave >= 33) %>% # First wave that question on e-cigarette in quit attempt 
  filter(trylyc == 1) %>% # Made at least one attempt to quit in past year
  filter(q632b7 >= 1)  # Made at least 1 SERIOUS attempt
  
# saveRDS(sts, "sts.RDS")
sts <- readRDS("sts.RDS") 


sts %>% tabyl(actage) 

attributes(sts$methodhier0715)

sts <- sts %>%
  filter(gore < 10 & actage >= 18) %>% # England and >=18
  mutate(any_aid2 = as.factor(case_when(methodhier0715 >= 3 ~ 1,
                                        methodhier0715 >= 0 ~ 0)),
         any_aid3 = as.factor(case_when(methodhier0715 >= 2 ~ 1, # includes "Any aid that is not meds or NHS model of support"
                                        methodhier0715 >= 0 ~ 0)))

addmargins(table(sts$any_aid2))

# Select on variables needed for analysis
sts <- sts %>%
  select() 

sts <- sts %>%
  mutate(sgz = factor(sgz),
         cigsmok = factor(cigsmok))

levels(sts$cigsmok)

model <- glm(cigsmok ~ sgz, data = sts, family = quasibinomial(link = "logit"), weights = weight0)
model <- glm(cigsmok ~ sgz, data = sts, family = binomial(link = "logit"), weights = weight0)
tidy(model, exp = T, conf.int = T)

svy_sts <- svydesign(ids = ~1, weights = ~weight0, data = as.data.frame(sts))

model2 <- svyglm(cigsmok ~ sgz, design = svy_sts)

tidy(model2, exp = T, conf.int = T)
