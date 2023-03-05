library(tidyverse)
library(data.table)
options(scipen = 999)
library(randomForest)
library(DALEX)
library(gbm)
#library(Boruta)
#library(rFerns)
library(mice)
library(VIM)
library(lattice)

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
      Infection  , data = Imputed_data, family=binomial()))

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

storage_30daymortality %>% filter(p.value<0.05)

model <- (glm(Death_30day ~ 
      Age  +
      ICU  +
      Chronic_pulm_disease  +
      Neo   +
      Charlson_comorbidity_index +
      Bilateral_pneumonia_or_ARDS   +
       Lymphocytes_count  +
       IMV_ecmo +
        CN_MV_MR_CPAP  +
        Dexamethasone_glucocorticoids   +
        KPC_EB_or_ACB_respiratory_tract_infection  +
        Length_of_stay  , data = Imputed_data, family=binomial()))

model <- tidy(model)

model$OR <- exp(model$estimate)
model$Upper <- exp(model$estimate + model$std.error)
model$Lower <- exp(model$estimate - model$std.error)

model <- model[,c(1,4,5,2,3,6,7,8)]
data.frame(model)


# ----------------------------
# Variable Importance with Boruta ---------------------------
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


Boruta(Death_30day~.,data=Imputed_data,doTrace=2)->Boruta.Imputed_data

print(Boruta.Imputed_data)
plot(Boruta.Imputed_data)

data.frame(Boruta.Imputed_data$finalDecision) %>%
  arrange(Boruta.Imputed_data.finalDecision)


plot(Boruta.Imputed_data, 
     colCode = c("violetred4", "violetred4", "steelblue4", "snow2"),
     notch = TRUE, las = 2, cex.axis=0.5, xlab="", ylab= "Relative Importance")


plotImpHistory(Boruta.Imputed_data)

# -------------------------------------------
# Classifiers --------------

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

# Imputed_data <- Imputed_data %>% mutate(new_Age = cut(Age, breaks=5)) %>% 
#   mutate(new_Age=paste0("Age_",new_Age)) %>% mutate(OneHot=1) %>%
#   spread(key=new_Age, value=OneHot) %>% select(-Age) %>%
#   replace(is.na(.), 0)
# 
# Imputed_data <- Imputed_data %>% mutate(new_Charlson_comorbidity_index = cut(Charlson_comorbidity_index, breaks=5)) %>% 
#   mutate(new_Charlson_comorbidity_index=paste0("Charlson_comorbidity_index_",new_Charlson_comorbidity_index)) %>% mutate(OneHot=1) %>%
#   spread(key=new_Charlson_comorbidity_index, value=OneHot) %>% select(-Charlson_comorbidity_index) %>%
#   replace(is.na(.), 0)
# 
# Imputed_data <- Imputed_data %>% mutate(new_D_dimer = cut(D_dimer, breaks=5)) %>% 
#   mutate(new_D_dimer=paste0("D_dimer_",new_D_dimer)) %>% mutate(OneHot=1) %>%
#   spread(key=new_D_dimer, value=OneHot) %>% select(-D_dimer) %>%
#   replace(is.na(.), 0)
# 
# Imputed_data <- Imputed_data %>% mutate(new_LDH = cut(LDH, breaks=5)) %>% 
#   mutate(new_LDH=paste0("LDH_",new_LDH)) %>% mutate(OneHot=1) %>%
#   spread(key=new_LDH, value=OneHot) %>% select(-LDH) %>%
#   replace(is.na(.), 0)
# 
# Imputed_data <- Imputed_data %>% mutate(new_CPK = cut(CPK, breaks=5)) %>% 
#   mutate(new_CPK=paste0("CPK_",new_CPK)) %>% mutate(OneHot=1) %>%
#   spread(key=new_CPK, value=OneHot) %>% select(-CPK) %>%
#   replace(is.na(.), 0)
# 
# Imputed_data <- Imputed_data %>% mutate(new_NT_proBNP = cut(NT_proBNP, breaks=5)) %>% 
#   mutate(new_NT_proBNP=paste0("NT_proBNP_",new_NT_proBNP)) %>% mutate(OneHot=1) %>%
#   spread(key=new_NT_proBNP, value=OneHot) %>% select(-NT_proBNP) %>%
#   replace(is.na(.), 0)
# 
# Imputed_data <- Imputed_data %>% mutate(new_Troponin_T = cut(Troponin_T, breaks=5)) %>% 
#   mutate(new_Troponin_T=paste0("Troponin_T_",new_Troponin_T)) %>% mutate(OneHot=1) %>%
#   spread(key=new_Troponin_T, value=OneHot) %>% select(-Troponin_T) %>%
#   replace(is.na(.), 0)
# 
# Imputed_data <- Imputed_data %>% mutate(new_Ferritin = cut(Ferritin, breaks=5)) %>% 
#   mutate(new_Ferritin=paste0("Ferritin_",new_Ferritin)) %>% mutate(OneHot=1) %>%
#   spread(key=new_Ferritin, value=OneHot) %>% select(-Ferritin) %>%
#   replace(is.na(.), 0)
# 
# Imputed_data <- Imputed_data %>% mutate(new_Creatinine = cut(Creatinine, breaks=5)) %>% 
#   mutate(new_Creatinine=paste0("Creatinine_",new_Creatinine)) %>% mutate(OneHot=1) %>%
#   spread(key=new_Creatinine, value=OneHot) %>% select(-Creatinine) %>%
#   replace(is.na(.), 0)
# 
# Imputed_data <- Imputed_data %>% mutate(new_Lymphocytes_count = cut(Lymphocytes_count, breaks=5)) %>% 
#   mutate(new_Lymphocytes_count=paste0("Lymphocytes_count_",new_Lymphocytes_count)) %>% mutate(OneHot=1) %>%
#   spread(key=new_Lymphocytes_count, value=OneHot) %>% select(-Lymphocytes_count) %>%
#   replace(is.na(.), 0)
# 
# Imputed_data <- Imputed_data %>% mutate(new_Procalcitonin = cut(Procalcitonin, breaks=5)) %>% 
#   mutate(new_Procalcitonin=paste0("Procalcitonin_",new_Procalcitonin)) %>% mutate(OneHot=1) %>%
#   spread(key=new_Procalcitonin, value=OneHot) %>% select(-Procalcitonin) %>%
#   replace(is.na(.), 0)
# 
# Imputed_data <- Imputed_data %>% mutate(new_CRP = cut(CRP, breaks=5)) %>% 
#   mutate(new_CRP=paste0("CRP_",new_CRP)) %>% mutate(OneHot=1) %>%
#   spread(key=new_CRP, value=OneHot) %>% select(-CRP) %>%
#   replace(is.na(.), 0)
# 
# Imputed_data <- Imputed_data %>% mutate(new_Length_of_stay = cut(Length_of_stay, breaks=5)) %>% 
#   mutate(new_Length_of_stay=paste0("Length_of_stay_",new_Length_of_stay)) %>% mutate(OneHot=1) %>%
#   spread(key=new_Length_of_stay, value=OneHot) %>% select(-Length_of_stay) %>%
#   replace(is.na(.), 0)
# 
# Imputed_data <- Imputed_data %>% mutate_if(is.numeric, as.factor)




temp <- as.matrix(
  Imputed_data %>% group_by(Death_30day, Infection) %>% 
    count() %>% ungroup() %>%
  spread(key=Infection, value=n))

matrix(as.numeric(c(temp[1,2], temp[1,3], temp[2,2], temp[2,3])), nrow=2) 

fisher.test( matrix(as.numeric(c(temp[1,2], temp[1,3], temp[2,2], temp[2,3])), nrow=2)  )

# Random forest 
modelAll_1_randomForest <- randomForest(Death_30day ~ . , data = Imputed_data, importance = F)

summary(modelAll_1_randomForest)

data.frame(modelAll_1_randomForest$importance) %>%
  arrange(-MeanDecreaseGini)

# Gradient Boost
modelAll_1_gbm <- gbm(Death_30day == 1 ~ . , data = Imputed_data, 
                n.trees = 15000, distribution = "bernoulli")

summary(modelAll_1_gbm, las=2, cex.lab=0.5)
