library(tidyverse)
library(data.table)
options(scipen = 999)
library(ISLR2)
library(leaps)
library(glmnet)
library(bestglm)


# library(randomForest)
# library(DALEX)
# library(gbm)
# library(Boruta)
# library(mice)
# library(VIM)
# library(lattice)
# library(broom)

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
# ----------------------------------

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

names(Imputed_data)[48] <- "y"



res.best.logistic <-
    bestglm(Xy = Imputed_data,
            family = binomial,          # binomial family for logistic
            IC = "AIC",                 # Information criteria for
            method = "exhaustive")



x <- model.matrix(Death ~ ., Imputed_data)[, -1]
y <- Imputed_data$Death


model <- regsubsets(Imputed_data$Death~. , data=Imputed_data, nvmax = 19)


cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial", type.measure ="auc")
plot(cv.lasso)
cv.lasso$cvm
cv.lasso$lambda
model <- glmnet(x, y, alpha = 1, family = "binomial", lambda = cv.lasso$lambda.1se, standardize =T)
coef(model)
model$beta

data.frame(coef(cv.lasso, cv.lasso$lambda.1se)

preds <- predict(cv.lasso, newx = x, type = 'response')
perf <- performance(prediction(preds, y), 'tpr', 'fpr')
plot(perf)

           


RegSubs <- fread("RegSubs.csv")
RegSubs[is.na(RegSubs)] <- 0


RegSubs %>% gather(Var, Pres, `Age (y)`:`ICU LoS`) %>%
  mutate(Pres=ifelse(Pres==1, "Yes", "No")) %>%
  rename("Predictor_Included"="Pres") %>%
  mutate(Predictor_Included=as.factor(Predictor_Included)) %>%
  ggplot(aes(x=V1 , y=Var, fill = Predictor_Included)) + 
  geom_tile(color = "white", size = 0.1) + 
  scale_fill_manual( values= c("snow", "deepskyblue4") ) +
  #scale_x_discrete(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) + 
  coord_equal() + 
  theme_minimal() +
  scale_x_continuous(breaks = seq(1,20,by=1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("\n Number of Predictors") +ylab("Predictor Included (yes/no) \n")



Lasso_Plot <- fread("Lasso_Plot.csv")
Lasso_Plot[is.na(Lasso_Plot)] <- 0

temp <- Lasso_Plot %>% transpose() %>% 
  bind_cols(colnames(Lasso_Plot)) %>%
  arrange(-abs(V1)) %>% mutate(V1=exp(V1))

names(temp)[1] <- "Relevant (?)"
names(temp)[2] <- "Predictor"

temp %>%
  mutate(`Relevant (?)`=ifelse(`Relevant (?)`==1,"No", "Yes")) %>%
  mutate(`Relevant (?)`=as.factor(`Relevant (?)`)) %>%
  ggplot(aes(x=`Relevant (?)` , y=Predictor, fill = `Relevant (?)`)) + 
  geom_tile(color = "white", size = 0.1) + 
  scale_fill_manual( values= c("snow", "deepskyblue4") ) +
  #scale_x_discrete(expand=c(0,0)) + 
  #scale_y_discrete(expand=c(0,0)) + 
  #coord_equal() + 
  theme_minimal() +
  scale_x_continuous(breaks = seq(1,20,by=1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("\n Number of Predictors") +ylab("Predictor Included (yes/no) \n")

