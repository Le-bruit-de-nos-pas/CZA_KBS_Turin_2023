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

# --------------------
# Univarite & Multivariate regression --------------------------------------------------------------

Imputed_data <- fread("Imputed_data.txt", sep="\t", colClasses = "character")

Imputed_data[ , "Age"] <- lapply(Imputed_data[ , "Age"], as.numeric)
Imputed_data[ , "Charlson_comorbidity_index"] <- lapply(Imputed_data[ , "Charlson_comorbidity_index"], as.numeric)
Imputed_data[ , "D_dimer"] <- lapply(Imputed_data[ , "D_dimer"], as.numeric)
Imputed_data[ , "LDH"] <- lapply(Imputed_data[ , "LDH"], as.numeric)
Imputed_data[ , "CPK"] <- lapply(Imputed_data[ , "CPK"], as.numeric)
Imputed_data[ , "NT_proBNP"] <- lapply(Imputed_data[ , "NT_proBNP"], as.numeric)
Imputed_data[ , "Troponin_T"] <- lapply(Imputed_data[ , "Troponin_T"], as.numeric)
Imputed_data[ , "Ferritin"] <- lapply(Imputed_data[ , "Ferritin"], as.numeric)
Imputed_data[ , "Creatinine"] <- lapply(Imputed_data[ , "Creatinine"], as.numeric)
Imputed_data[ , "Lymphocytes_count"] <- lapply(Imputed_data[ , "Lymphocytes_count"], as.numeric)
Imputed_data[ , "Procalcitonin"] <- lapply(Imputed_data[ , "Procalcitonin"], as.numeric)
Imputed_data[ , "CRP"] <- lapply(Imputed_data[ , "CRP"], as.numeric)
Imputed_data[ , "Length_of_stay"] <- lapply(Imputed_data[ , "Length_of_stay"], as.numeric)

Imputed_data <- Imputed_data %>% mutate_if(is.character, as.factor)

# Imputed_data <- Imputed_data %>% mutate_if(is.numeric, scale)


# --------------------------------------
# Rectal Carriers  Table 1 & Table 2  --------------------------------------------------------------

names(Imputed_data)

storage_rectal_carriers <- data.frame()

for(i in names(Imputed_data)[-36]){
  print(i)
  cat(" - - - - - - - - - - - - - - - - - - - - - - - - -")
  model <- glm(Rectal_carriage ~ get(i), data = Imputed_data, family=binomial())
  storage_rectal_carriers <- rbind(storage_rectal_carriers, tidy(model)[2,])
}


storage_rectal_carriers <- data.frame(
  storage_rectal_carriers %>% 
             bind_cols(
               data.frame(names(Imputed_data)[-36])
               )
  )  

storage_rectal_carriers <- storage_rectal_carriers[,-1]

names(storage_rectal_carriers)[5] <- "feature"

storage_rectal_carriers <- storage_rectal_carriers[,c(5,3,4,1,2)]

storage_rectal_carriers$OR <- exp(storage_rectal_carriers$estimate)
storage_rectal_carriers$Upper <- exp(storage_rectal_carriers$estimate + storage_rectal_carriers$std.error)
storage_rectal_carriers$Lower <- exp(storage_rectal_carriers$estimate - storage_rectal_carriers$std.error)

storage_rectal_carriers$OR <- exp(storage_rectal_carriers$estimate)

storage_rectal_carriers %>% filter(p.value<0.05)

model <- (glm(Rectal_carriage ~ 
      ICU+
      Neo+Diabetes+
      Pulmonary_embolism+
      IMV_ecmo+
      Hydroxychloroquine+
      Lopinavir_ritonavir+
      Infection+
      KPC_EB_or_ACB_respiratory_tract_infection  , data = Imputed_data, family=binomial()))

model <- tidy(model)

model$OR <- exp(model$estimate)
model$Upper <- exp(model$estimate + model$std.error)
model$Lower <- exp(model$estimate - model$std.error)

model <- model[,c(1,4,5,2,3,6,7,8)]

# ---------------------------------------------------------------------------------------------
# Infection Table 3 & Table 4  --------------------------------------------------------------


names(Imputed_data)

storage_infection <- data.frame()

for(i in names(Imputed_data)[-37]){
  print(i)
  cat(" - - - - - - - - - - - - - - - - - - - - - - - - -")
  model <- glm(Infection ~ get(i), data = Imputed_data, family=binomial())
  storage_infection <- rbind(storage_infection, tidy(model)[2,])
}


storage_infection <- data.frame(
  storage_infection %>% 
             bind_cols(
               data.frame(names(Imputed_data)[-37])
               )
  )  

storage_infection <- storage_infection[,-1]

names(storage_infection)[5] <- "feature"

storage_infection <- storage_infection[,c(5,3,4,1,2)]

storage_infection$OR <- exp(storage_infection$estimate)
storage_infection$Upper <- exp(storage_infection$estimate + storage_infection$std.error)
storage_infection$Lower <- exp(storage_infection$estimate - storage_infection$std.error)


storage_infection %>% filter(p.value<0.05)

model <- (glm(Infection ~ 
      Pulmonary_embolism+
        CPK+
      Ferritin+
      Lymphocytes_count+
      CRP+
      IMV_ecmo , data = Imputed_data, family=binomial()))

model <- tidy(model)

model$OR <- exp(model$estimate)
model$Upper <- exp(model$estimate + model$std.error)
model$Lower <- exp(model$estimate - model$std.error)

model <- model[,c(1,4,5,2,3,6,7,8)]
data.frame(model)

# --------------------------------------------------------------------------
# 30 day mortality Table 5 & Table 6  --------------------------------------------------------------


names(Imputed_data)

storage_30daymortality <- data.frame()

for(i in names(Imputed_data)[-43]){
  print(i)
  cat(" - - - - - - - - - - - - - - - - - - - - - - - - -")
  model <- glm(Death_30day ~ get(i), data = Imputed_data, family=binomial())
  storage_30daymortality <- rbind(storage_30daymortality, tidy(model)[2,])
}


storage_30daymortality <- data.frame(
  storage_30daymortality %>% 
             bind_cols(
               data.frame(names(Imputed_data)[-43])
               )
  )  

storage_30daymortality <- storage_30daymortality[,-1]

names(storage_30daymortality)[5] <- "feature"

storage_30daymortality <- storage_30daymortality[,c(5,3,4,1,2)]

storage_30daymortality$OR <- exp(storage_30daymortality$estimate)
storage_30daymortality$Upper <- exp(storage_30daymortality$estimate + storage_30daymortality$std.error)
storage_30daymortality$Lower <- exp(storage_30daymortality$estimate - storage_30daymortality$std.error)

storage_30daymortality$Lower <- round(storage_30daymortality$Lower,digits=5)

storage_infection %>% filter(p.value<0.05)

model <- (glm(Death_30day ~ 
      ICU  +
      Pulmonary_embolism  +
      CPK  +
      Ferritin  +
      Lymphocytes_count +
      CRP  +
       IMV_ecmo  +
       Rectal_carriage   , data = Imputed_data, family=binomial()))

model <- tidy(model)

model$OR <- exp(model$estimate)
model$Upper <- exp(model$estimate + model$std.error)
model$Lower <- exp(model$estimate - model$std.error)

model <- model[,c(1,4,5,2,3,6,7,8)]
data.frame(model)


# ----------------------------