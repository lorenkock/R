library(survey)
#install.packages("srvyr");
library(srvyr)
library(tidyverse)

# Read in data 2019 to 2022
nhis19 <- read.csv("/Users/lkock/Desktop/UVM/NHIS/adult19.csv")
nhis20 <- read.csv("/Users/lkock/Desktop/UVM/NHIS/adult20.csv")
nhis21 <- read.csv("/Users/lkock/Desktop/UVM/NHIS/adult21.csv")
nhis22 <- read.csv("/Users/lkock/Desktop/UVM/NHIS/adult22.csv")

# Function to create dual use, exlusive smoker and exclusive vaper variables
tob_status <- function(df) {
  # Dual use 
  df <- df %>%
    mutate(dual_use = as.factor(case_when(SMKCIGST_A <= 2 & SMKECIGST_A == 1 ~ 1,
                                          TRUE ~ 0)))
  
  # Exclusive e-cigarette user
  df <- df %>%
    mutate(excl_ecig = as.factor(case_when((SMKCIGST_A == 3 | SMKCIGST_A == 4) & SMKECIGST_A == 1 ~ 1,
                                           TRUE ~ 0)))
  
  # Exclusive cigarette smoker
  df <- df %>%
    mutate(excl_cig = as.factor(case_when(SMKCIGST_A <= 2 & SMKECIGST_A != 1 ~ 1,
                                          TRUE ~ 0)))
  
}

# Run function on each dataset to create variables
nhis19 <- tob_status(nhis19)
nhis20 <- tob_status(nhis20)
nhis21 <- tob_status(nhis21)
nhis22 <- tob_status(nhis22)

######
# 2019 
######

# Svy design
nhis19_svy <- svydesign(id = ~PPSU, 
                        strata = ~PSTRAT,
                        nest = TRUE,
                        weights = ~WTFA_A,
                        data = nhis19)


addmargins(table(nhis19$dual_use))
addmargins(table(nhis19$SMKCIGST_A))
addmargins(table(nhis19$SMKECIGST_A))

## Subset whole USA

# Dual use
svymean(~dual_use, design = nhis19_svy, na.rm = T)
confint(svymean(~dual_use, design = nhis19_svy, na.rm = T))

svyby(~dual_use, ~SEX_A, svymean, design = nhis19_svy, na.rm = T)
confint(svyby(~dual_use, ~SEX_A, svymean, design = nhis19_svy, na.rm = T))

# Exclusive e-cig use
svymean(~excl_ecig, design = nhis19_svy)
confint(svymean(~excl_ecig, design = nhis19_svy, na.rm = T))

svyby(~excl_ecig, ~SEX_A, svymean, design = nhis19_svy, na.rm = T)
confint(svyby(~excl_ecig, ~SEX_A, svymean, design = nhis19_svy, na.rm = T))

# Exclusive smoking
svymean(~excl_cig, design = nhis19_svy)
confint(svymean(~excl_cig, design = nhis19_svy, na.rm = T))

svyby(~excl_cig, ~SEX_A, svymean, design = nhis19_svy, na.rm = T)
confint(svyby(~excl_cig, ~SEX_A, svymean, design = nhis19_svy, na.rm = T))


## Subset current smokers
nhis19_svy_cig <- subset(nhis19_svy, SMKCIGST_A <= 2)

svymean(~dual_use, design = nhis19_svy_cig)
confint(svymean(~dual_use, design = nhis19_svy_cig, na.rm = T))

svyby(~dual_use, ~SEX_A, svymean, design = nhis19_svy_cig, na.rm = T)
confint(svyby(~dual_use, ~SEX_A, svymean, design = nhis19_svy_cig, na.rm = T))


## Subset current e-cig users
nhis19_svy_ec <- subset(nhis19_svy, SMKECIGST_A == 1)

svymean(~dual_use, design = nhis19_svy_ec)
confint(svymean(~dual_use, design = nhis19_svy_ec, na.rm = T))

svyby(~dual_use, ~SEX_A, svymean, design = nhis19_svy_ec, na.rm = T)
confint(svyby(~dual_use, ~SEX_A, svymean, design = nhis19_svy_ec, na.rm = T))



######
# 2020
######

# Svy design
nhis20_svy <- svydesign(id = ~PPSU, 
                        strata = ~PSTRAT,
                        nest = TRUE,
                        weights = ~WTFA_A,
                        data = nhis20)

addmargins(table(nhis20$SMKECIGST_A))

## Subset whole USA

# Dual use
svymean(~dual_use, design = nhis20_svy, na.rm = T)
confint(svymean(~dual_use, design = nhis20_svy, na.rm = T))

svyby(~dual_use, ~SEX_A, svymean, design = nhis20_svy, na.rm = T)
confint(svyby(~dual_use, ~SEX_A, svymean, design = nhis20_svy, na.rm = T))

# Exclusive e-cig use
svymean(~excl_ecig, design = nhis20_svy)
confint(svymean(~excl_ecig, design = nhis20_svy, na.rm = T))

svyby(~excl_ecig, ~SEX_A, svymean, design = nhis20_svy, na.rm = T)
confint(svyby(~excl_ecig, ~SEX_A, svymean, design = nhis20_svy, na.rm = T))

# Exclusive smoking
svymean(~excl_cig, design = nhis20_svy)
confint(svymean(~excl_cig, design = nhis20_svy, na.rm = T))

svyby(~excl_cig, ~SEX_A, svymean, design = nhis20_svy, na.rm = T)
confint(svyby(~excl_cig, ~SEX_A, svymean, design = nhis20_svy, na.rm = T))


## Subset current smokers
nhis20_svy_cig <- subset(nhis20_svy, SMKCIGST_A <= 2)

svymean(~dual_use, design = nhis20_svy_cig)
confint(svymean(~dual_use, design = nhis20_svy_cig, na.rm = T))

svyby(~dual_use, ~SEX_A, svymean, design = nhis20_svy_cig, na.rm = T)
confint(svyby(~dual_use, ~SEX_A, svymean, design = nhis20_svy_cig, na.rm = T))


## Subset current e-cig users
nhis20_svy_ec <- subset(nhis20_svy, SMKECIGST_A == 1)

svymean(~dual_use, design = nhis20_svy_ec)
confint(svymean(~dual_use, design = nhis20_svy_ec, na.rm = T))

svyby(~dual_use, ~SEX_A, svymean, design = nhis20_svy_ec, na.rm = T)
confint(svyby(~dual_use, ~SEX_A, svymean, design = nhis20_svy_ec, na.rm = T))


######
# 2021
######

# Svy design
nhis21_svy <- svydesign(id = ~PPSU, 
                        strata = ~PSTRAT,
                        nest = TRUE,
                        weights = ~WTFA_A,
                        data = nhis21)

nrow(nhis21)

addmargins(table(nhis21$dual_use))
addmargins(table(nhis21$SMKCIGST_A))
addmargins(table(nhis21$SMKECIGST_A))

## Subset whole USA

# Dual use
svymean(~dual_use, design = nhis21_svy, na.rm = T)
confint(svymean(~dual_use, design = nhis21_svy, na.rm = T))

svyby(~dual_use, ~SEX_A, svymean, design = nhis21_svy, na.rm = T)
confint(svyby(~dual_use, ~SEX_A, svymean, design = nhis21_svy, na.rm = T))

# Exclusive e-cig use
svymean(~excl_ecig, design = nhis21_svy)
confint(svymean(~excl_ecig, design = nhis21_svy, na.rm = T))

svyby(~excl_ecig, ~SEX_A, svymean, design = nhis21_svy, na.rm = T)
confint(svyby(~excl_ecig, ~SEX_A, svymean, design = nhis21_svy, na.rm = T))

# Exclusive smoking
svymean(~excl_cig, design = nhis21_svy)
confint(svymean(~excl_cig, design = nhis21_svy, na.rm = T))

svyby(~excl_cig, ~SEX_A, svymean, design = nhis21_svy, na.rm = T)
confint(svyby(~excl_cig, ~SEX_A, svymean, design = nhis21_svy, na.rm = T))


## Subset current smokers
nhis21_svy_cig <- subset(nhis21_svy, SMKCIGST_A <= 2)

svymean(~dual_use, design = nhis21_svy_cig)
confint(svymean(~dual_use, design = nhis21_svy_cig, na.rm = T))

svyby(~dual_use, ~SEX_A, svymean, design = nhis21_svy_cig, na.rm = T)
confint(svyby(~dual_use, ~SEX_A, svymean, design = nhis21_svy_cig, na.rm = T))


## Subset current e-cig users
nhis21_svy_ec <- subset(nhis21_svy, SMKECIGST_A == 1)

svymean(~dual_use, design = nhis21_svy_ec)
confint(svymean(~dual_use, design = nhis21_svy_ec, na.rm = T))

svyby(~dual_use, ~SEX_A, svymean, design = nhis21_svy_ec, na.rm = T)
confint(svyby(~dual_use, ~SEX_A, svymean, design = nhis21_svy_ec, na.rm = T))


######
# 2022
######

# Svy design
nhis22_svy <- svydesign(id = ~PPSU, 
                        strata = ~PSTRAT,
                        nest = TRUE,
                        weights = ~WTFA_A,
                        data = nhis22)

nrow(nhis22)

addmargins(table(nhis22$dual_use))
addmargins(table(nhis22$SMKCIGST_A))
addmargins(table(nhis22$SMKECIGST_A))

## Subset whole USA

# Dual use
svymean(~dual_use, design = nhis22_svy, na.rm = T)
confint(svymean(~dual_use, design = nhis22_svy, na.rm = T))

svyby(~dual_use, ~SEX_A, svymean, design = nhis22_svy, na.rm = T)
confint(svyby(~dual_use, ~SEX_A, svymean, design = nhis22_svy, na.rm = T))

# Exclusive e-cig use
svymean(~excl_ecig, design = nhis22_svy)
confint(svymean(~excl_ecig, design = nhis22_svy, na.rm = T))

svyby(~excl_ecig, ~SEX_A, svymean, design = nhis22_svy, na.rm = T)
confint(svyby(~excl_ecig, ~SEX_A, svymean, design = nhis22_svy, na.rm = T))

# Exclusive smoking
svymean(~excl_cig, design = nhis22_svy)
confint(svymean(~excl_cig, design = nhis22_svy, na.rm = T))

svyby(~excl_cig, ~SEX_A, svymean, design = nhis22_svy, na.rm = T)
confint(svyby(~excl_cig, ~SEX_A, svymean, design = nhis22_svy, na.rm = T))


## Subset current smokers
nhis22_svy_cig <- subset(nhis22_svy, SMKCIGST_A <= 2)

svymean(~dual_use, design = nhis22_svy_cig)
confint(svymean(~dual_use, design = nhis22_svy_cig, na.rm = T))

svyby(~dual_use, ~SEX_A, svymean, design = nhis22_svy_cig, na.rm = T)
confint(svyby(~dual_use, ~SEX_A, svymean, design = nhis22_svy_cig, na.rm = T))


## Subset current e-cig users
nhis22_svy_ec <- subset(nhis22_svy, SMKECIGST_A == 1)

svymean(~dual_use, design = nhis22_svy_ec)
confint(svymean(~dual_use, design = nhis22_svy_ec, na.rm = T))

svyby(~dual_use, ~SEX_A, svymean, design = nhis22_svy_ec, na.rm = T)
confint(svyby(~dual_use, ~SEX_A, svymean, design = nhis22_svy_ec, na.rm = T))
