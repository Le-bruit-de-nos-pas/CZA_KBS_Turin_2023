library(tidyverse)
library(data.table)
library(mice)
library(VIM)
library(lattice)
options(scipen = 999)

# Data cleaning ---------------------------------------------------------------------------------------------

KBS_COVID_Boattini23 <- fread("KBS_COVID_Boattini23.txt", sep = "\t", colClasses = "character")

data.frame(colSums(KBS_COVID_Boattini23==""))

KBS_COVID_Boattini23 <- KBS_COVID_Boattini23 %>% mutate(CN_MV_MR_CPAP = ifelse(CN_MV_MR_CPAP=="2","0", CN_MV_MR_CPAP)) 
KBS_COVID_Boattini23 <- KBS_COVID_Boattini23 %>% mutate(Obesity = ifelse(Obesity=="","0", Obesity)) 
KBS_COVID_Boattini23 <- KBS_COVID_Boattini23 %>% mutate(Pulmonary_embolism = ifelse(Pulmonary_embolism=="11","1", Pulmonary_embolism)) 
KBS_COVID_Boattini23 <- KBS_COVID_Boattini23 %>% mutate(Pulmonary_embolism = ifelse(Pulmonary_embolism=="11","1", Pulmonary_embolism)) 

KBS_COVID_Boattini23[ , "Age"] <- lapply(KBS_COVID_Boattini23[ , "Age"], as.numeric)
KBS_COVID_Boattini23[ , "Charlson_comorbidity_index"] <- lapply(KBS_COVID_Boattini23[ , "Charlson_comorbidity_index"], as.numeric)
KBS_COVID_Boattini23[ , "D_dimer"] <- lapply(KBS_COVID_Boattini23[ , "D_dimer"], as.numeric)
KBS_COVID_Boattini23[ , "LDH"] <- lapply(KBS_COVID_Boattini23[ , "LDH"], as.numeric)
KBS_COVID_Boattini23[ , "CPK"] <- lapply(KBS_COVID_Boattini23[ , "CPK"], as.numeric)
KBS_COVID_Boattini23[ , "NT_proBNP"] <- lapply(KBS_COVID_Boattini23[ , "NT_proBNP"], as.numeric)
KBS_COVID_Boattini23[ , "Troponin_T"] <- lapply(KBS_COVID_Boattini23[ , "Troponin_T"], as.numeric)
KBS_COVID_Boattini23[ , "Ferritin"] <- lapply(KBS_COVID_Boattini23[ , "Ferritin"], as.numeric)
KBS_COVID_Boattini23[ , "Creatinine"] <- lapply(KBS_COVID_Boattini23[ , "Creatinine"], as.numeric)
KBS_COVID_Boattini23[ , "Lymphocytes_count"] <- lapply(KBS_COVID_Boattini23[ , "Lymphocytes_count"], as.numeric)
KBS_COVID_Boattini23[ , "Procalcitonin"] <- lapply(KBS_COVID_Boattini23[ , "Procalcitonin"], as.numeric)
KBS_COVID_Boattini23[ , "CRP"] <- lapply(KBS_COVID_Boattini23[ , "CRP"], as.numeric)
KBS_COVID_Boattini23[ , "Length_of_stay"] <- lapply(KBS_COVID_Boattini23[ , "Length_of_stay"], as.numeric)

data.frame(colSums(is.na(KBS_COVID_Boattini23)))

KBS_COVID_Boattini23 <- KBS_COVID_Boattini23 %>% mutate_if(is.character, as.factor)

# --------------------------------------------------------------------------------------------

# Input missing values --------------------------------------------------------------------------


KBS_COVID_Boattini23 <- fread("KBS_COVID_Boattini23.txt", sep = "\t", colClasses = "character")

data.frame(colSums(KBS_COVID_Boattini23==""))

KBS_COVID_Boattini23 <- KBS_COVID_Boattini23 %>% mutate(CN_MV_MR_CPAP = ifelse(CN_MV_MR_CPAP=="2","0", CN_MV_MR_CPAP)) 
KBS_COVID_Boattini23 <- KBS_COVID_Boattini23 %>% mutate(Obesity = ifelse(Obesity=="","0", Obesity)) 
KBS_COVID_Boattini23 <- KBS_COVID_Boattini23 %>% mutate(Pulmonary_embolism = ifelse(Pulmonary_embolism=="11","1", Pulmonary_embolism)) 
KBS_COVID_Boattini23 <- KBS_COVID_Boattini23 %>% mutate(Pulmonary_embolism = ifelse(Pulmonary_embolism=="11","1", Pulmonary_embolism)) 

KBS_COVID_Boattini23[ , "Age"] <- lapply(KBS_COVID_Boattini23[ , "Age"], as.numeric)
KBS_COVID_Boattini23[ , "Charlson_comorbidity_index"] <- lapply(KBS_COVID_Boattini23[ , "Charlson_comorbidity_index"], as.numeric)
KBS_COVID_Boattini23[ , "D_dimer"] <- lapply(KBS_COVID_Boattini23[ , "D_dimer"], as.numeric)
KBS_COVID_Boattini23[ , "LDH"] <- lapply(KBS_COVID_Boattini23[ , "LDH"], as.numeric)
KBS_COVID_Boattini23[ , "CPK"] <- lapply(KBS_COVID_Boattini23[ , "CPK"], as.numeric)
KBS_COVID_Boattini23[ , "NT_proBNP"] <- lapply(KBS_COVID_Boattini23[ , "NT_proBNP"], as.numeric)
KBS_COVID_Boattini23[ , "Troponin_T"] <- lapply(KBS_COVID_Boattini23[ , "Troponin_T"], as.numeric)
KBS_COVID_Boattini23[ , "Ferritin"] <- lapply(KBS_COVID_Boattini23[ , "Ferritin"], as.numeric)
KBS_COVID_Boattini23[ , "Creatinine"] <- lapply(KBS_COVID_Boattini23[ , "Creatinine"], as.numeric)
KBS_COVID_Boattini23[ , "Lymphocytes_count"] <- lapply(KBS_COVID_Boattini23[ , "Lymphocytes_count"], as.numeric)
KBS_COVID_Boattini23[ , "Procalcitonin"] <- lapply(KBS_COVID_Boattini23[ , "Procalcitonin"], as.numeric)
KBS_COVID_Boattini23[ , "CRP"] <- lapply(KBS_COVID_Boattini23[ , "CRP"], as.numeric)
KBS_COVID_Boattini23[ , "Length_of_stay"] <- lapply(KBS_COVID_Boattini23[ , "Length_of_stay"], as.numeric)

data.frame(colSums(is.na(KBS_COVID_Boattini23))) # 188 -> 131

KBS_COVID_Boattini23 <- KBS_COVID_Boattini23 %>% mutate_if(is.character, as.factor)

md.pattern(KBS_COVID_Boattini23, rotate.names = TRUE)

ignore <- KBS_COVID_Boattini23[,18:23]

temp_miss = aggr(ignore, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, combined=FALSE, only.miss=TRUE,
                 labels=names(ignore), cex.axis=.7, gap=1, 
                 ylab=c("Proportion of missingness","Missingness Pattern"))

KBS_COVID_Boattini23_complete <- mice(KBS_COVID_Boattini23, m=50, maxit = 40)

data.frame(KBS_COVID_Boattini23_complete$method)

# pmm, predictive mean matching (numeric data) 
# logreg, logistic regression imputation (binary data, factor with 2 levels) 
# polyreg, polytomous regression imputation for unordered categorical data (factor > 2 levels) 
# polr, proportional odds model for (ordered, > 2 levels)

 
# D_dimer                                                                    pmm
# LDH                                                                        pmm
# CPK                                                                        pmm
# NT_proBNP                                                                  pmm
# Troponin_T                                                                 pmm
# Ferritin                                                                   pmm

Imputed_data=complete(KBS_COVID_Boattini23_complete)

densityplot(KBS_COVID_Boattini23_complete)

# COVID  - - - -  - - - - - - - -  -- - - - - - -

# D dimers Imputed 
KBS_COVID_Boattini23 %>% group_by(Community_adquired_SARS_CoV_2_infection) %>% summarise(n=mean(D_dimer, na.rm = TRUE ))
Imputed_data %>% group_by(Community_adquired_SARS_CoV_2_infection) %>% summarise(n=mean(D_dimer, na.rm = TRUE ))

# LDH Imputed 
KBS_COVID_Boattini23 %>% group_by(Community_adquired_SARS_CoV_2_infection) %>% summarise(n=mean(LDH, na.rm = TRUE ))
Imputed_data %>% group_by(Community_adquired_SARS_CoV_2_infection) %>% summarise(n=mean(LDH, na.rm = TRUE ))

# CPK
KBS_COVID_Boattini23 %>% group_by(Community_adquired_SARS_CoV_2_infection) %>% summarise(n=mean(CPK, na.rm = TRUE ))
Imputed_data %>% group_by(Community_adquired_SARS_CoV_2_infection) %>% summarise(n=mean(CPK, na.rm = TRUE ))

# NT_proBNP
KBS_COVID_Boattini23 %>% group_by(Community_adquired_SARS_CoV_2_infection) %>% summarise(n=mean(NT_proBNP, na.rm = TRUE ))
Imputed_data %>% group_by(Community_adquired_SARS_CoV_2_infection) %>% summarise(n=mean(NT_proBNP, na.rm = TRUE ))

# Troponin_T
KBS_COVID_Boattini23 %>% group_by(Community_adquired_SARS_CoV_2_infection) %>% summarise(n=mean(Troponin_T, na.rm = TRUE ))
Imputed_data %>% group_by(Community_adquired_SARS_CoV_2_infection) %>% summarise(n=mean(Troponin_T, na.rm = TRUE ))

# Ferritin
KBS_COVID_Boattini23 %>% group_by(Community_adquired_SARS_CoV_2_infection) %>% summarise(n=mean(Ferritin, na.rm = TRUE ))
Imputed_data %>% group_by(Community_adquired_SARS_CoV_2_infection) %>% summarise(n=mean(Ferritin, na.rm = TRUE ))

# Death - - - -  - - - - - - - -  -- - - - - - -

# D dimers Imputed 
KBS_COVID_Boattini23 %>% group_by(Death_30day) %>% summarise(n=mean(D_dimer, na.rm = TRUE ))
Imputed_data %>% group_by(Death_30day) %>% summarise(n=mean(D_dimer, na.rm = TRUE ))

# LDH Imputed 
KBS_COVID_Boattini23 %>% group_by(Death_30day) %>% summarise(n=mean(LDH, na.rm = TRUE ))
Imputed_data %>% group_by(Death_30day) %>% summarise(n=mean(LDH, na.rm = TRUE ))

# CPK
KBS_COVID_Boattini23 %>% group_by(Death_30day) %>% summarise(n=mean(CPK, na.rm = TRUE ))
Imputed_data %>% group_by(Death_30day) %>% summarise(n=mean(CPK, na.rm = TRUE ))

# NT_proBNP
KBS_COVID_Boattini23 %>% group_by(Death_30day) %>% summarise(n=mean(NT_proBNP, na.rm = TRUE ))
Imputed_data %>% group_by(Death_30day) %>% summarise(n=mean(NT_proBNP, na.rm = TRUE ))

# Troponin_T
KBS_COVID_Boattini23 %>% group_by(Death_30day) %>% summarise(n=mean(Troponin_T, na.rm = TRUE ))
Imputed_data %>% group_by(Death_30day) %>% summarise(n=mean(Troponin_T, na.rm = TRUE ))

# Ferritin
KBS_COVID_Boattini23 %>% group_by(Death_30day) %>% summarise(n=mean(Ferritin, na.rm = TRUE ))
Imputed_data %>% group_by(Death_30day) %>% summarise(n=mean(Ferritin, na.rm = TRUE ))


# summary(glm(Death_30day ~  D_dimer+LDH+CPK+NT_proBNP+Troponin_T+Ferritin,  data = Imputed_data, family = "binomial"))

fwrite(Imputed_data, "Imputed_data.txt", sep="\t")

# -------------------------------------------------------------------------------------------------







summary(glm( Community_adquired_SARS_CoV_2_infection1  ~ ., data = Imputed_data, family = binomial))
