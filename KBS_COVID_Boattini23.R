library(tidyverse)
library(data.table)
options(scipen = 999)
library(randomForest)
library(DALEX)
library(gbm)
library(Boruta)
library(mice)
library(VIM)
library(lattice)
library(broom)

# Data cleaning ---------------------------------------------------------------------------------------------

KBS_COVID_Boattini23 <- fread("KBS_COVID_Boattini23_v2.txt", sep = "\t", colClasses = "character")

data.frame(colSums(KBS_COVID_Boattini23==""))

KBS_COVID_Boattini23 <- KBS_COVID_Boattini23 %>% mutate(CN_MV_MR_CPAP = ifelse(CN_MV_MR_CPAP=="2","0", CN_MV_MR_CPAP)) 
KBS_COVID_Boattini23 <- KBS_COVID_Boattini23 %>% mutate(Obesity = ifelse(Obesity=="","0", Obesity)) 
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
KBS_COVID_Boattini23[ , "LoS"] <- lapply(KBS_COVID_Boattini23[ , "LoS"], as.numeric)
KBS_COVID_Boattini23[ , "ICU_LoS"] <- lapply(KBS_COVID_Boattini23[ , "ICU_LoS"], as.numeric)

data.frame(colSums(is.na(KBS_COVID_Boattini23)))

KBS_COVID_Boattini23 <- KBS_COVID_Boattini23 %>% mutate_if(is.character, as.factor)

# --------------------------------------------------------------------------------------------

# Input missing values --------------------------------------------------------------------------

KBS_COVID_Boattini23 <- fread("KBS_COVID_Boattini23_v2.txt", sep = "\t", colClasses = "character")

data.frame(colSums(KBS_COVID_Boattini23==""))

sum(KBS_COVID_Boattini23=="")

KBS_COVID_Boattini23 <- KBS_COVID_Boattini23 %>% mutate(CN_MV_MR_CPAP = ifelse(CN_MV_MR_CPAP=="2","0", CN_MV_MR_CPAP)) 
KBS_COVID_Boattini23 <- KBS_COVID_Boattini23 %>% mutate(Obesity = ifelse(Obesity=="","0", Obesity)) 
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
KBS_COVID_Boattini23[ , "LoS"] <- lapply(KBS_COVID_Boattini23[ , "LoS"], as.numeric)
KBS_COVID_Boattini23[ , "ICU_LoS"] <- lapply(KBS_COVID_Boattini23[ , "ICU_LoS"], as.numeric)

data.frame(colSums(is.na(KBS_COVID_Boattini23)))

KBS_COVID_Boattini23 <- KBS_COVID_Boattini23 %>% mutate_if(is.character, as.factor)

md.pattern(KBS_COVID_Boattini23, rotate.names = TRUE)

names(KBS_COVID_Boattini23)

ignore <- KBS_COVID_Boattini23[,17:22]

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
KBS_COVID_Boattini23 %>% group_by(Death) %>% summarise(n=mean(D_dimer, na.rm = TRUE ))
Imputed_data %>% group_by(Death) %>% summarise(n=mean(D_dimer, na.rm = TRUE ))

# LDH Imputed 
KBS_COVID_Boattini23 %>% group_by(Death) %>% summarise(n=mean(LDH, na.rm = TRUE ))
Imputed_data %>% group_by(Death) %>% summarise(n=mean(LDH, na.rm = TRUE ))

# CPK
KBS_COVID_Boattini23 %>% group_by(Death) %>% summarise(n=mean(CPK, na.rm = TRUE ))
Imputed_data %>% group_by(Death) %>% summarise(n=mean(CPK, na.rm = TRUE ))

# NT_proBNP
KBS_COVID_Boattini23 %>% group_by(Death) %>% summarise(n=mean(NT_proBNP, na.rm = TRUE ))
Imputed_data %>% group_by(Death) %>% summarise(n=mean(NT_proBNP, na.rm = TRUE ))

# Troponin_T
KBS_COVID_Boattini23 %>% group_by(Death) %>% summarise(n=mean(Troponin_T, na.rm = TRUE ))
Imputed_data %>% group_by(Death) %>% summarise(n=mean(Troponin_T, na.rm = TRUE ))

# Ferritin
KBS_COVID_Boattini23 %>% group_by(Death) %>% summarise(n=mean(Ferritin, na.rm = TRUE ))
Imputed_data %>% group_by(Death) %>% summarise(n=mean(Ferritin, na.rm = TRUE ))



fwrite(Imputed_data, "Imputed_data.txt", sep="\t")

# --------------------
# Univarite & Multivariate regression --------------------------------------------------------------

Imputed_data <- fread("Imputed_data.txt", sep="\t", colClasses = "character")

data.frame(colSums(Imputed_data==""))

Imputed_data <- Imputed_data %>% mutate(CN_MV_MR_CPAP = ifelse(CN_MV_MR_CPAP=="2","0", CN_MV_MR_CPAP)) 
Imputed_data <- Imputed_data %>% mutate(Obesity = ifelse(Obesity=="","0", Obesity)) 
Imputed_data <- Imputed_data %>% mutate(Pulmonary_embolism = ifelse(Pulmonary_embolism=="11","1", Pulmonary_embolism)) 

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
Imputed_data[ , "LoS"] <- lapply(Imputed_data[ , "LoS"], as.numeric)
Imputed_data[ , "ICU_LoS"] <- lapply(Imputed_data[ , "ICU_LoS"], as.numeric)

data.frame(colSums(is.na(Imputed_data)))

Imputed_data <- Imputed_data %>% mutate_if(is.character, as.factor)


# --------------------------------------
# Factors associated with Death  --------------------------------------------------------------

names(Imputed_data)

storage_Death <- data.frame()

for(i in names(Imputed_data)[-48]){
  print(i)
  cat(" - - - - - - - - - - - - - - - - - - - - - - - - -")
  model <- glm(Death ~ get(i), data = Imputed_data, family=binomial())
  storage_Death <- rbind(storage_Death, tidy(model)[2,])
}


storage_Death <- data.frame(
  storage_Death %>% 
             bind_cols(
               data.frame(names(Imputed_data)[-48])
               )
  )  

storage_Death <- storage_Death[,-1]

names(storage_Death)[5] <- "feature"

storage_Death <- storage_Death[,c(5,3,4,1,2)]

storage_Death$OR <- exp(storage_Death$estimate)
storage_Death$Upper <- exp(storage_Death$estimate + storage_Death$std.error)
storage_Death$Lower <- exp(storage_Death$estimate - storage_Death$std.error)

storage_Death$Lower <- round(storage_Death$Lower,digits=5)
storage_Death$Upper <- round(storage_Death$Upper,digits=5)
storage_Death$OR <- round(storage_Death$OR,digits=5)
storage_Death$p.value <- round(storage_Death$p.value,digits=5)
storage_Death$statistic<- round(storage_Death$statistic,digits=5)
storage_Death$estimate<- round(storage_Death$estimate,digits=5)
storage_Death$std.error<- round(storage_Death$std.error,digits=5)

storage_Death %>% filter(p.value<0.05)

#                          feature statistic       p.value    estimate   std.error        OR     Upper   Lower
# 1                            Age  3.747678 0.00017847897  0.05679188 0.015153883 1.0584355 1.0745971 1.04252
# 2      Chronic_pulmonary_disease  2.416307 0.01567885263  0.87448046 0.361907905 2.3976293 3.4431556 1.66958
# 3                            Neo  2.245741 0.02472059793  0.87422727 0.389282311 2.3970223 3.5378160 1.62409
# 4     Charlson_comorbidity_index  3.522457 0.00042756594  0.24230006 0.068787224 1.2741765 1.3649083 1.18948
# 5    Bilateral_pneumonia_or_ARDS  2.451877 0.01421131887  1.40761701 0.574097701 4.0862064 7.2551728 2.30140
# 6              Lymphocytes_count -2.820339 0.00479729739 -0.71869914 0.254827237 0.4873859 0.6288441 0.37775
# 7                       IMV_ecmo  3.903681 0.00009474049  1.20122177 0.307715117 3.3241758 4.5219209 2.44368
# 8                  CN_MV_MR_CPAP  2.383670 0.01714097110  1.16575159 0.489057483 3.2083333 5.2320809 1.96736
# 9  Dexamethasone_glucocorticoids  3.149660 0.00163460379  1.42138568 0.451282209 4.1428571 6.5056296 2.63822
# 10                           LoS -2.787445 0.00531254853 -0.01777746 0.006377689 0.9823796 0.9886650 0.97613
# 11                           ICU  2.460833 0.01386148481  0.76595222 0.311257287 2.1510417 2.9364739 1.57569
# 12                       ICU_LoS  1.991214 0.04645740473  0.02368782 0.011896173 1.0239706 1.0362247 1.01186


model <- (glm(Death ~ 
      Age  +
      Chronic_pulmonary_disease  +
      Neo  +
      Charlson_comorbidity_index   +
      Bilateral_pneumonia_or_ARDS +
      Lymphocytes_count   +
       Lymphocytes_count  +
       IMV_ecmo +
        CN_MV_MR_CPAP  +
        Dexamethasone_glucocorticoids  + 
        LoS +
        ICU + 
        ICU_LoS, data = Imputed_data, family=binomial()))

model <- tidy(model)

model$OR <- exp(model$estimate)
model$Upper <- exp(model$estimate + model$std.error)
model$Lower <- exp(model$estimate - model$std.error)

model <- model[,c(1,4,5,2,3,6,7,8)]
data.frame(model)

#                              term  statistic       p.value    estimate  std.error          OR      Upper        Lower
# 1                     (Intercept) -4.0772907 0.00004556352 -6.75862756 1.65762712 0.001160821 0.00609065 0.0002212417
# 2                             Age  1.8712603 0.06130900928  0.04036137 0.02156908 1.041186961 1.06388835 1.0189699731
# 3      Chronic_pulmonary_disease1  1.3220196 0.18616162015  0.61939917 0.46852495 1.857811477 2.96810623 1.1628503881
# 4                            Neo1  1.4876132 0.13685293935  0.85659702 0.57581974 2.355132584 4.18881034 1.3241586622
# 5      Charlson_comorbidity_index  2.6393717 0.00830598450  0.30294674 0.11477987 1.353842350 1.51850546 1.2070349154
# 6    Bilateral_pneumonia_or_ARDS1  0.3068933 0.75892460051  0.23147727 0.75425982 1.260460679 2.67978640 0.5928685681
# 7               Lymphocytes_count -2.0660579 0.03882301160 -0.59564539 0.28830043 0.551206710 0.73539688 0.4131494792
# 8                       IMV_ecmo1  3.8816417 0.00010375365  1.78301348 0.45934519 5.947752853 9.41551320 3.7571785247
# 9                  CN_MV_MR_CPAP1  0.9255362 0.35468708062  0.63614721 0.68732832 1.889188202 3.75645439 0.9501065887
# 10 Dexamethasone_glucocorticoids1  2.2011190 0.02772759716  1.31166308 0.59590738 3.712342513 6.73670182 2.0457320654


# ----------------------------
# Variable Importance with Boruta ---------------------------
Imputed_data <- fread("Imputed_data.txt", sep="\t", colClasses = "character")

data.frame(colSums(Imputed_data==""))

Imputed_data <- Imputed_data %>% mutate(CN_MV_MR_CPAP = ifelse(CN_MV_MR_CPAP=="2","0", CN_MV_MR_CPAP)) 
Imputed_data <- Imputed_data %>% mutate(Obesity = ifelse(Obesity=="","0", Obesity)) 
Imputed_data <- Imputed_data %>% mutate(Pulmonary_embolism = ifelse(Pulmonary_embolism=="11","1", Pulmonary_embolism)) 

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
Imputed_data[ , "LoS"] <- lapply(Imputed_data[ , "LoS"], as.numeric)
Imputed_data[ , "ICU_LoS"] <- lapply(Imputed_data[ , "ICU_LoS"], as.numeric)

data.frame(colSums(is.na(Imputed_data)))

Imputed_data <- Imputed_data %>% mutate_if(is.character, as.factor)


Boruta(Death~.,data=Imputed_data,doTrace=2) -> Boruta.Imputed_data

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

data.frame(colSums(Imputed_data==""))

Imputed_data <- Imputed_data %>% mutate(CN_MV_MR_CPAP = ifelse(CN_MV_MR_CPAP=="2","0", CN_MV_MR_CPAP)) 
Imputed_data <- Imputed_data %>% mutate(Obesity = ifelse(Obesity=="","0", Obesity)) 
Imputed_data <- Imputed_data %>% mutate(Pulmonary_embolism = ifelse(Pulmonary_embolism=="11","1", Pulmonary_embolism)) 

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
Imputed_data[ , "LoS"] <- lapply(Imputed_data[ , "LoS"], as.numeric)
Imputed_data[ , "ICU_LoS"] <- lapply(Imputed_data[ , "ICU_LoS"], as.numeric)

data.frame(colSums(is.na(Imputed_data)))

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
  Imputed_data %>% group_by(Death, Colonized_KPC_only) %>% 
    count() %>% ungroup() %>%
  spread(key=Colonized_KPC_only, value=n))

matrix(as.numeric(c(temp[1,2], temp[1,3], temp[2,2], temp[2,3])), nrow=2) 

fisher.test( matrix(as.numeric(c(temp[1,2], temp[1,3], temp[2,2], temp[2,3])), nrow=2)  )




Imputed_data_factors <- Imputed_data %>% select(where(is.factor))

# Random forest 
modelAll_1_randomForest <- randomForest(Death ~ . , data = Imputed_data_factors, importance = F)

summary(modelAll_1_randomForest)

data.frame(modelAll_1_randomForest$importance) %>%
  arrange(-MeanDecreaseGini)

# Gradient Boost
modelAll_1_gbm <- gbm(Death == 1 ~ . , data = Imputed_data_factors, 
                n.trees = 15000, distribution = "bernoulli")

summary(modelAll_1_gbm, las=2, cex.lab=0.5)

# ------------------------
# Summary statistics ------------------
Imputed_data <- fread("Imputed_data.txt", sep="\t", colClasses = "character")

data.frame(colSums(Imputed_data==""))

Imputed_data <- Imputed_data %>% mutate(CN_MV_MR_CPAP = ifelse(CN_MV_MR_CPAP=="2","0", CN_MV_MR_CPAP)) 
Imputed_data <- Imputed_data %>% mutate(Obesity = ifelse(Obesity=="","0", Obesity)) 
Imputed_data <- Imputed_data %>% mutate(Pulmonary_embolism = ifelse(Pulmonary_embolism=="11","1", Pulmonary_embolism)) 

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
Imputed_data[ , "LoS"] <- lapply(Imputed_data[ , "LoS"], as.numeric)
Imputed_data[ , "ICU_LoS"] <- lapply(Imputed_data[ , "ICU_LoS"], as.numeric)

data.frame(colSums(is.na(Imputed_data)))

Imputed_data <- Imputed_data %>% mutate_if(is.character, as.factor)

Storage_means <- data.frame()

for(i in names(Imputed_data)[-48]){
  print(i)
  cat(" - - - - - - - - - - - - - - - - - - - - - - - - -")
  x <- mean(Imputed_data[,get(i)])
  Storage_means <- rbind(Storage_means, x)
}

IQR(Imputed_data$Age)
shapiro.test(Imputed_data$ICU_LoS)


for(i in names(Imputed_data_factors)){
  print(i)
  cat(" - - - - - - - - - - - - - - - - - - - - - - - - -")
  print(sum(Imputed_data_factors[,get(i)]==1))
}


Imputed_data %>% group_by(Death, Death) %>% count()



temp <- as.matrix(
  Imputed_data %>% group_by(Death, IMV_ecmo) %>% 
    count() %>% ungroup() %>%
  spread(key=IMV_ecmo, value=n))

matrix(as.numeric(c(temp[1,2], temp[1,3], temp[2,2], temp[2,3])), nrow=2) 

fisher.test( matrix(as.numeric(c(temp[1,2], temp[1,3], temp[2,2], temp[2,3])), nrow=2)  )


wilcox.test(LoS~Death, data = Imputed_data)

# ------------------

Imputed_data <- Imputed_data %>% mutate_if(is.factor, as.numeric)

Imputed_data <- Imputed_data %>%        
  mutate_all( ~(scale(.) %>% as.vector))

prcompvdata <- prcomp(Imputed_data[c(1:47)], center = TRUE, scale = F)
summary(prcompvdata)

ignore <- prcompvdata$rotation

my.var = varimax(prcompvdata$rotation)

myvarshort <-my.var$loadings[,1:12]
myvarshort <- as.data.frame(myvarshort)


Imputed_data