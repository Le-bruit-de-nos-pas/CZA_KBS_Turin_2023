library(tidyverse)
library(data.table)
library(broom)
library(randomForest)
library(factoextra)
library(cluster)
options(scipen = 999)


# Import data - Sanity Checks ------------------------------------------------------------------------------

KBS_CZA_BoattiniM_2023 <- fread("KBS_CZA_BoattiniM_2023.txt", sep = "\t")

KBS_CZA_BoattiniM_2023 %>% drop_na() # 0 left

names(KBS_CZA_BoattiniM_2023)


for(i in 1:length(KBS_CZA_BoattiniM_2023)) {
  if( 
    
    count(distinct(KBS_CZA_BoattiniM_2023[, ..i])) == 2 |  
    (count(distinct(KBS_CZA_BoattiniM_2023[, ..i])) == 3 & sum(is.na(distinct(KBS_CZA_BoattiniM_2023[, ..i]))) == 1) ) {
  } else {
    print(i)
  }
}


KBS_CZA_BoattiniM_2023[, ] <- lapply(KBS_CZA_BoattiniM_2023[ , ], as.factor)
KBS_CZA_BoattiniM_2023[, 1] <- lapply(KBS_CZA_BoattiniM_2023[ , 1], as.numeric)
KBS_CZA_BoattiniM_2023[, 12] <- lapply(KBS_CZA_BoattiniM_2023[ , 12], as.numeric)
KBS_CZA_BoattiniM_2023[, 18] <- lapply(KBS_CZA_BoattiniM_2023[ , 18], as.numeric)
KBS_CZA_BoattiniM_2023[, 19] <- lapply(KBS_CZA_BoattiniM_2023[ , 19], as.numeric)
KBS_CZA_BoattiniM_2023[, 20] <- lapply(KBS_CZA_BoattiniM_2023[ , 20], as.numeric)
KBS_CZA_BoattiniM_2023[, 21] <- lapply(KBS_CZA_BoattiniM_2023[ , 21], as.numeric)
KBS_CZA_BoattiniM_2023[, 35] <- lapply(KBS_CZA_BoattiniM_2023[ , 35], as.numeric)
KBS_CZA_BoattiniM_2023[, 40] <- lapply(KBS_CZA_BoattiniM_2023[ , 40], as.numeric)


# -----------------------------------------------------------------------------------


# Summary Mean/Median/SD/etc -----------------------------------------------------------------------

SummaryTable <- describe(KBS_CZA_BoattiniM_2023)




# -----------------------------------------------------------------------------------

# Logit Regression ------------------------------------------------------------------------

KBS_CZA_BoattiniM_2023 <- fread("KBS_CZA_BoattiniM_2023.txt", sep = "\t")

data.frame(colSums(is.na(KBS_CZA_BoattiniM_2023)))
  
KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023[, -c("TIGE_susceptible")]
KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023[, -c("MEV_susceptibility")]

KBS_CZA_BoattiniM_2023$D30_mortality <- as.factor(KBS_CZA_BoattiniM_2023$D30_mortality)
KBS_CZA_BoattiniM_2023$In_hospital_death <- as.factor(KBS_CZA_BoattiniM_2023$In_hospital_death)

temp <- KBS_CZA_BoattiniM_2023[,-72]

# Check for Singularities precluding us from running a logit

# cortemp <- cor(temp[,-72], method="spearman")
# cortemp <- as.data.frame(cortemp)
# cortemp$Var1 <- row.names(cortemp)
# cortemp <- gather(cortemp,Var2, score,  Age:CZA_exposure)

# temp %>% group_by(`INCREMENT_≥_8`, `INCREMENT_≥_9`, `INCREMENT_≥_10`, `INCREMENT_≥_11`) %>% count()
# temp <- temp %>% select(-c(`INCREMENT_≥_9`, `INCREMENT_≥_10`))
# summary(glm(D30_mortality ~  `INCREMENT_≥_11` + Shock , data = temp, family=binomial()))













# --------------------------------------------------------------------------------------------------------------------------- 


# Logistic Regression 30-day Mortality  -------------------------------------------------------------------------------------



KBS_CZA_BoattiniM_2023 <- fread("KBS_CZA_BoattiniM_2023.txt", sep = "\t")

data.frame(colSums(is.na(KBS_CZA_BoattiniM_2023)))
  
KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% mutate(Prolonged_B_lact_infusion = ifelse(is.na(Prolonged_B_lact_infusion), 0, Prolonged_B_lact_infusion))
KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% mutate(KPC_Infection_relapse = ifelse(is.na(KPC_Infection_relapse), 0, KPC_Infection_relapse))
KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% mutate(Dyalisis_30day_preceding = ifelse(is.na(Dyalisis_30day_preceding), 1, Dyalisis_30day_preceding))

KBS_CZA_BoattiniM_2023[, ] <- lapply(KBS_CZA_BoattiniM_2023[ , ], as.factor)
KBS_CZA_BoattiniM_2023[, 1] <- lapply(KBS_CZA_BoattiniM_2023[ , 1], as.numeric)
KBS_CZA_BoattiniM_2023[, 12] <- lapply(KBS_CZA_BoattiniM_2023[ , 12], as.numeric)
KBS_CZA_BoattiniM_2023[, 18] <- lapply(KBS_CZA_BoattiniM_2023[ , 18], as.numeric)
KBS_CZA_BoattiniM_2023[, 19] <- lapply(KBS_CZA_BoattiniM_2023[ , 19], as.numeric)
KBS_CZA_BoattiniM_2023[, 20] <- lapply(KBS_CZA_BoattiniM_2023[ , 20], as.numeric)
KBS_CZA_BoattiniM_2023[, 21] <- lapply(KBS_CZA_BoattiniM_2023[ , 21], as.numeric)
KBS_CZA_BoattiniM_2023[, 35] <- lapply(KBS_CZA_BoattiniM_2023[ , 35], as.numeric)
KBS_CZA_BoattiniM_2023[, 40] <- lapply(KBS_CZA_BoattiniM_2023[ , 40], as.numeric)

KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023[, -c("TIGE_susceptible")]
KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023[, -c("MEV_susceptibility")]

KBS_CZA_BoattiniM_2023$D30_mortality[is.na(KBS_CZA_BoattiniM_2023$D30_mortality)] <- 0

KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% select(-c(Charlson_big3,
                                     Charlsonbig4, 
                                     Charlsonbig5, 
                                     Charlsonbig6,
                                     PITT_scorebig2,
                                     PITT_scorebig3,
                                     PITT_scorebig4,
                                     PITT_scorebig5,
                                     INCREMENT_big_8,
                                     INCREMENT_big_9,
                                     INCREMENT_big_10,
                                     INCREMENT_big_11))


KBS_CZA_BoattiniM_2023$D30_mortality <- as.factor(KBS_CZA_BoattiniM_2023$D30_mortality)
KBS_CZA_BoattiniM_2023$In_hospital_death <- as.factor(KBS_CZA_BoattiniM_2023$In_hospital_death)



temp <- KBS_CZA_BoattiniM_2023[,-60]

names(temp)


storage <- data.frame()

for(i in names(temp)[1:59]){

    print(i)
  
  cat(" - - - - - - - - - - - - - - - - - - - - - - - - -")
  
  model <- glm(D30_mortality ~ get(i), data = temp, family=binomial())
  
  storage <- rbind(storage, tidy(model)[2,])
}


data.frame(storage)

storage$term <- names(temp)[-60]

data.frame(storage)


storage$Expestimate <- exp(storage$estimate)
storage$Explow <- exp(storage$estimate - storage$std.error)
storage$Exphigh <- exp(storage$estimate + storage$std.error)

storage <- storage[,c(1,5,6,7,8)]
data.frame(storage)

storage <- data.frame(storage %>% drop_na() %>% filter(Exphigh != "Inf"))

storage$term <- as.factor(storage$term)

storage %>%
  mutate(p.value = ifelse(p.value<0.05, "<0.05","ns")) %>%
  mutate(p.value=as.factor(p.value)) %>%
  ggplot(aes(y=fct_reorder(term, Expestimate), x=Expestimate, xmin=Explow, xmax=Exphigh, colour=p.value)) +
  geom_errorbarh(height=.05,, size=3, alpha=0.7) +
  geom_point( size=3, alpha=0.6) + 
  labs(title='Univariate Logistic Regression - Odd Ratio for 30-day Mortality', 
       x='\n Log10 Odd Ratio', y = 'Predictor \n') +
  geom_vline(xintercept=1, color='black', linetype='dashed', size=1 , alpha=.7) +
  scale_x_log10() +
  theme_minimal() +
  scale_colour_manual(values = c("lightpink3", "lightskyblue4"))




# ---------------------------------------------------------------------------------------------------------------------------






# Logistic Regression - Each Variable Individually --------------------------------------------------------------------------

for(i in names(temp)[36:71]){
  
  print(i)
  
  cat(" - - - - - - - - - - - - - - - - - - - - - - - - -")
  
  print(summary(glm(D30_mortality ~ get(i), data = temp, family=binomial())))

  }


# ---------------------------------------------------------------------------------------------------------------------------


# Logistic Regression In-hospital mortality --------------------------------------------------------------------------------


KBS_CZA_BoattiniM_2023 <- fread("KBS_CZA_BoattiniM_2023.txt", sep = "\t")

data.frame(colSums(is.na(KBS_CZA_BoattiniM_2023)))
  
KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% mutate(Prolonged_B_lact_infusion = ifelse(is.na(Prolonged_B_lact_infusion), 0, Prolonged_B_lact_infusion))
KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% mutate(KPC_Infection_relapse = ifelse(is.na(KPC_Infection_relapse), 0, KPC_Infection_relapse))
KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% mutate(Dyalisis_30day_preceding = ifelse(is.na(Dyalisis_30day_preceding), 1, Dyalisis_30day_preceding))

KBS_CZA_BoattiniM_2023[, ] <- lapply(KBS_CZA_BoattiniM_2023[ , ], as.factor)
KBS_CZA_BoattiniM_2023[, 1] <- lapply(KBS_CZA_BoattiniM_2023[ , 1], as.numeric)
KBS_CZA_BoattiniM_2023[, 12] <- lapply(KBS_CZA_BoattiniM_2023[ , 12], as.numeric)
KBS_CZA_BoattiniM_2023[, 18] <- lapply(KBS_CZA_BoattiniM_2023[ , 18], as.numeric)
KBS_CZA_BoattiniM_2023[, 19] <- lapply(KBS_CZA_BoattiniM_2023[ , 19], as.numeric)
KBS_CZA_BoattiniM_2023[, 20] <- lapply(KBS_CZA_BoattiniM_2023[ , 20], as.numeric)
KBS_CZA_BoattiniM_2023[, 21] <- lapply(KBS_CZA_BoattiniM_2023[ , 21], as.numeric)
KBS_CZA_BoattiniM_2023[, 35] <- lapply(KBS_CZA_BoattiniM_2023[ , 35], as.numeric)
KBS_CZA_BoattiniM_2023[, 40] <- lapply(KBS_CZA_BoattiniM_2023[ , 40], as.numeric)

KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023[, -c("TIGE_susceptible")]
KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023[, -c("MEV_susceptibility")]

KBS_CZA_BoattiniM_2023$D30_mortality[is.na(KBS_CZA_BoattiniM_2023$D30_mortality)] <- 0

KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% select(-c(Charlson_big3,
                                     Charlsonbig4, 
                                     Charlsonbig5, 
                                     Charlsonbig6,
                                     PITT_scorebig2,
                                     PITT_scorebig3,
                                     PITT_scorebig4,
                                     PITT_scorebig5,
                                     INCREMENT_big_8,
                                     INCREMENT_big_9,
                                     INCREMENT_big_10,
                                     INCREMENT_big_11))


KBS_CZA_BoattiniM_2023$D30_mortality <- as.factor(KBS_CZA_BoattiniM_2023$D30_mortality)
KBS_CZA_BoattiniM_2023$In_hospital_death <- as.factor(KBS_CZA_BoattiniM_2023$In_hospital_death)



temp <- KBS_CZA_BoattiniM_2023[,-61]

names(temp)


storage <- data.frame()

for(i in names(temp)[1:59]){

    print(i)
  
  cat(" - - - - - - - - - - - - - - - - - - - - - - - - -")
  
  model <- glm(In_hospital_death ~ get(i), data = temp, family=binomial())
  
  storage <- rbind(storage, tidy(model)[2,])
}


data.frame(storage)

storage$term <- names(temp)[-60]

data.frame(storage)


storage$Expestimate <- exp(storage$estimate)
storage$Explow <- exp(storage$estimate - storage$std.error)
storage$Exphigh <- exp(storage$estimate + storage$std.error)

storage <- storage[,c(1,5,6,7,8)]
data.frame(storage)

storage <- data.frame(storage %>% drop_na() %>% filter(Exphigh != "Inf"))

storage$term <- as.factor(storage$term)

storage %>%
  mutate(p.value = ifelse(p.value<0.05, "<0.05","ns")) %>%
  mutate(p.value=as.factor(p.value)) %>%
  ggplot(aes(y=fct_reorder(term, Expestimate), x=Expestimate, xmin=Explow, xmax=Exphigh, colour=p.value)) +
  geom_errorbarh(height=.05,, size=3, alpha=0.7) +
  geom_point( size=3, alpha=0.6) + 
  labs(title='Univariate Logistic Regression - Odd Ratio for In-hospital Mortality', 
       x='\n Log10 Odd Ratio', y = 'Predictor \n') +
  geom_vline(xintercept=1, color='black', linetype='dashed', size=1 , alpha=.7) +
  scale_x_log10() +
  theme_minimal() +
  scale_colour_manual(values = c("lightpink3", "lightskyblue4"))


# ---------------------------------------------------------------------------------------------------------------------------

# Logistic Regression - Each Variable Individually ----------------------------------------------------------------------------

for(i in names(temp)[1:35]){
  
  print(i)
  
  cat(" - - - - - - - - - - - - - - - - - - - - - - - - -")
  
  print(summary(glm(In_hospital_death ~ get(i), data = temp, family=binomial())))

  }



# ------------------------------------------------------------------------------------------------------------------------------------

# Random Forest - Explanatory Modeling -----------------------------------------------------------------------------------------


KBS_CZA_BoattiniM_2023 <- fread("KBS_CZA_BoattiniM_2023.txt", sep = "\t", colClasses = "character")

data.frame(colSums(is.na(KBS_CZA_BoattiniM_2023)))
  
KBS_CZA_BoattiniM_2023$D30_mortality[KBS_CZA_BoattiniM_2023$D30_mortality==""] <- 0

KBS_CZA_BoattiniM_2023$D30_mortality <- as.factor(KBS_CZA_BoattiniM_2023$D30_mortality)
KBS_CZA_BoattiniM_2023$In_hospital_death <- as.factor(KBS_CZA_BoattiniM_2023$In_hospital_death)

#KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% mutate(Prolonged_B_lact_infusion = ifelse(is.na(Prolonged_B_lact_infusion), 0, Prolonged_B_lact_infusion))
#KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% mutate(KPC_Infection_relapse = ifelse(is.na(KPC_Infection_relapse), 0, KPC_Infection_relapse))
#KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% mutate(Dyalisis_30day_preceding = ifelse(is.na(Dyalisis_30day_preceding), 1, Dyalisis_30day_preceding))

KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% mutate(Prolonged_B_lact_infusion = ifelse(Prolonged_B_lact_infusion=="", 0, Prolonged_B_lact_infusion))
KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% mutate(KPC_Infection_relapse = ifelse(KPC_Infection_relapse=="", 0, KPC_Infection_relapse))
KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% mutate(Dyalisis_30day_preceding = ifelse(Dyalisis_30day_preceding=="", 1, Dyalisis_30day_preceding))


KBS_CZA_BoattiniM_2023[, 1] <- lapply(KBS_CZA_BoattiniM_2023[ , 1], as.numeric)
KBS_CZA_BoattiniM_2023[, 12] <- lapply(KBS_CZA_BoattiniM_2023[ , 12], as.numeric)
KBS_CZA_BoattiniM_2023[, 18] <- lapply(KBS_CZA_BoattiniM_2023[ , 18], as.numeric)
KBS_CZA_BoattiniM_2023[, 19] <- lapply(KBS_CZA_BoattiniM_2023[ , 19], as.numeric)
KBS_CZA_BoattiniM_2023[, 20] <- lapply(KBS_CZA_BoattiniM_2023[ , 20], as.numeric)
KBS_CZA_BoattiniM_2023[, 21] <- lapply(KBS_CZA_BoattiniM_2023[ , 21], as.numeric)
KBS_CZA_BoattiniM_2023[, 35] <- lapply(KBS_CZA_BoattiniM_2023[ , 35], as.numeric)
KBS_CZA_BoattiniM_2023[, 40] <- lapply(KBS_CZA_BoattiniM_2023[ , 40], as.numeric)

KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% mutate_if(is.character, as.factor)
                                     
KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023[, -c("TIGE_susceptible")]
KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023[, -c("MEV_susceptibility")]



KBS_CZA_BoattiniM_2023 %>% group_by(In_hospital_death) %>% summarise(n=mean(PITT_score)) 
KBS_CZA_BoattiniM_2023 %>% group_by(In_hospital_death, COL_susceptible) %>% count()

KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% select(-c(Charlson_big3,
                                     Charlsonbig4, 
                                     Charlsonbig5, 
                                     Charlsonbig6,
                                     PITT_scorebig2,
                                     PITT_scorebig3,
                                     PITT_scorebig4,
                                     PITT_scorebig5,
                                     INCREMENT_big_8,
                                     INCREMENT_big_9,
                                     INCREMENT_big_10,
                                     INCREMENT_big_11))

# -----------------------------------------------------------------------------
# In hospital mortality -----------------------------------------------------------------------------------------

temp <- KBS_CZA_BoattiniM_2023[,-61]

names(temp)

temp <- temp %>% drop_na()

temp$In_hospital_death <- as.factor(temp$In_hospital_death)

library(randomForest)




modelAll_1_randomForest <- randomForest(In_hospital_death ~ . , data = temp)
# 500 trees, 7 predictors sampled at each node

print(modelAll_1_randomForest)


summary(modelAll_1_randomForest)

modelAll_1_randomForest$importance


# number_hits <- list()
# class_tested <- list()
# 
# for(i in 1:37) {
#   temp2 <- temp[-i,]
#   modelAll_1_randomForest <- randomForest(In_hospital_death ~ . , data = temp2, mtry=37, ntree=10000)
#   predict <- predict(modelAll_1_randomForest, temp[i,], type = 'response')
#   number_hits <- append(number_hits, data.frame(predict)[1] == data.frame(temp[i,60]))
#   class_tested <- append(class_tested, data.frame(temp[i,60])[1])
#   number_hits <- unlist(number_hits)
#   class_tested <- unlist(class_tested)
# }
# 
# 
# data.frame(number_hits) %>%
#   bind_cols(data.frame(class_tested)) %>%
#   group_by(class_tested, number_hits) %>% count()
# 
# number_hits <- 1*(data.frame(number_hits))
# 
# scores <- number_hits %>%
#   bind_cols(data.frame(class_tested))
# 
# class_tested <- data.frame(class_tested)
# 
# names(number_hits)[1] <- "class"
# names(class_tested)[1] <- "class"
# 
# scores <- number_hits %>% bind_cols(class_tested)
# scores$class...2 <- as.numeric(scores$class...2)
# roc.plot(scores$class...2, scores$class...1)







library(DALEX)
explainer_ranger <- explain(modelAll_1_randomForest,
                            data = temp,
                            y =  temp$In_hospital_death,
                            label = "model_RandomForest")

print(explainer_ranger)
plot(explainer_ranger)
describe(explainer_ranger)

# Dead
new_observation <- temp[8,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)
new_observation <- temp[8,]
bd_ranger <- predict_profile(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)



new_observation <- temp[10,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)
new_observation <- temp[10,]
bd_ranger <- predict_profile(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)


new_observation <- temp[11,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)
new_observation <- temp[11,]
bd_ranger <- predict_profile(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)



new_observation <- temp[17,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)
new_observation <- temp[17,]
bd_ranger <- predict_profile(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)



new_observation <- temp[20,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)
new_observation <- temp[20,]
bd_ranger <- predict_profile(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)


new_observation <- temp[25,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)
new_observation <- temp[25,]
bd_ranger <- predict_profile(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)

new_observation <- temp[30,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)
new_observation <- temp[30,]
bd_ranger <- predict_profile(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)



new_observation <- temp[32,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)
new_observation <- temp[32,]
bd_ranger <- predict_profile(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)


# Alive
new_observation <- temp[9,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)
new_observation <- temp[9,]
bd_ranger <- predict_profile(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)

new_observation <- temp[12,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)
new_observation <- temp[12,]
bd_ranger <- predict_profile(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)

new_observation <- temp[13,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)
new_observation <- temp[13,]
bd_ranger <- predict_profile(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)

new_observation <- temp[14,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)
new_observation <- temp[13,]
bd_ranger <- predict_profile(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)

new_observation <- temp[15,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)

new_observation <- temp[35,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)

new_observation <- temp[36,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)


new_observation <- temp[37,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)



# -----------------------------------------------------------------------------

# 30-day Mortality  -----------------------------------------------------------------------------------------

temp <- KBS_CZA_BoattiniM_2023[,-60]

names(temp)

temp <- temp %>% drop_na()

temp$D30_mortality <- as.factor(temp$D30_mortality)

library(randomForest)

modelAll_1_randomForest <- randomForest(D30_mortality ~ . , data = temp)

summary(modelAll_1_randomForest)

modelAll_1_randomForest$importance

explainer_ranger <- explain(modelAll_1_randomForest,
                            data = temp,
                            y =  temp$D30_mortality,
                            label = "model_RandomForest")

# Dead
new_observation <- temp[8,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)

new_observation <- temp[10,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)

new_observation <- temp[17,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)

new_observation <- temp[20,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)

new_observation <- temp[25,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)

new_observation <- temp[32,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)


# Alive
new_observation <- temp[9,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)

new_observation <- temp[11,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)

new_observation <- temp[18,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)

new_observation <- temp[22,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)

new_observation <- temp[26,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)

new_observation <- temp[34,]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)














# --------------------------------------------------------------------------------------------------

# k-means clustering ---------------------------------------------------------------------------
# 
# temp
# 
# temp[, ] <- lapply(temp[ , ], as.numeric)
# #temp$D30_mortality <- as.factor(temp$D30_mortality)
# 
# names(temp)
# temp <- temp %>% select(4,5,6,12,21,25,31,32,35,56,60)
# 
# df2 <- scale(temp[,])
# 
# set.seed(1)
# 
# km <- kmeans(df2, centers = 2, nstart = 100)
# 
# km # K-means clustering with 2 clusters of sizes 77493, 13507  ~15%
# 
# fviz_cluster(km, data = df2, labelsize = 1, ggtheme = theme_minimal(), axes = c(1, 2), pointsize =1, ellipse.alpha = 0.9) +
#   ggsci::scale_color_jco(palette = "default") +
#   ggsci::scale_fill_jco(palette = "default") 
# #  xlim(-2,10)
# 
# 
# aggregate(temp[,], by=list(cluster=km$cluster), mean)
# 
# #   cluster       DM      CVD      CKD Charlson_Index Cardiosurgery Dyalisis_30day_preceding PITT_score CPE_INCREMENT
# # 1       1 1.357143 1.714286 1.500000       5.714286      1.571429                 1.642857   5.571429      4.714286
# # 2       2 1.173913 1.130435 1.086957       3.565217      1.000000                 1.086957   2.869565      2.478261
# #        AKI Adjusted_for_renal_function D30_mortality
# # 1 1.857143                    1.428571      1.428571
# # 2 1.000000                    1.043478      1.000000





# validation 

# create_train_test <- function(data, size = 0.8, train = TRUE) {
#   n_row = nrow(data)
#   total_row = size * n_row
#   train_sample <- 1: total_row
#   if (train == TRUE) {
#     return (data[train_sample, ])
#   } else {
#     return (data[-train_sample, ])
#   }
# }
# 
# data_train <- temp %>% group_by(In_hospital_death) %>% sample_n(6) %>% ungroup()
# data_test <- temp %>% group_by(In_hospital_death) %>% sample_n(2) %>% ungroup()

data_test <- temp %>% group_by(In_hospital_death) %>% sample_n(4) %>% ungroup()
modelAll_1_randomForest <- randomForest(In_hospital_death ~ . , data = temp)

predict <- predict(modelAll_1_randomForest, data_test, type = 'response')

table_mat <- table(data_test$In_hospital_death, predict)
table_mat

# 
# plot(table_mat)
# 
# accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
# accuracy_Test
# 
# precision <- function(matrix) {
#   # True positive
#   tp <- matrix[2, 2]
#   # false positive
#   fp <- matrix[1, 2]
#   return (tp / (tp + fp))
# }
# 
# recall <- function(matrix) {
#   # true positive
#   tp <- matrix[2, 2]# false positive
#   fn <- matrix[2, 1]
#   return (tp / (tp + fn))
# }
# 
# prec <- precision(table_mat)
# prec
# 
# rec <- recall(table_mat)
# rec 
# 
# f1 <- 2 * ((prec * rec) / (prec + rec))
# f1 

# ---------------------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------------------

# Plot Summary distributions -------------------------------------------------
# Plot some distribution

KBS_CZA_BoattiniM_2023 <- fread("KBS_CZA_BoattiniM_2023.txt", sep = "\t")

KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% mutate(Prolonged_B_lact_infusion = ifelse(is.na(Prolonged_B_lact_infusion), 0, Prolonged_B_lact_infusion))
KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% mutate(KPC_Infection_relapse = ifelse(is.na(KPC_Infection_relapse), 0, KPC_Infection_relapse))
KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% mutate(Dyalisis_30day_preceding = ifelse(is.na(Dyalisis_30day_preceding), 1, Dyalisis_30day_preceding))

KBS_CZA_BoattiniM_2023[, ] <- lapply(KBS_CZA_BoattiniM_2023[ , ], as.numeric)

KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023[, -c("TIGE_susceptible")]
KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023[, -c("MEV_susceptibility")]

KBS_CZA_BoattiniM_2023$D30_mortality[is.na(KBS_CZA_BoattiniM_2023$D30_mortality)] <- 0

KBS_CZA_BoattiniM_2023 %>% group_by(In_hospital_death) %>% summarise(n=mean(PITT_score)) 
KBS_CZA_BoattiniM_2023 %>% group_by(In_hospital_death, COL_susceptible) %>% count()

KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% select(-c(Charlson_big3,
                                     Charlsonbig4, 
                                     Charlsonbig5, 
                                     Charlsonbig6,
                                     PITT_scorebig2,
                                     PITT_scorebig3,
                                     PITT_scorebig4,
                                     PITT_scorebig5,
                                     INCREMENT_big_8,
                                     INCREMENT_big_9,
                                     INCREMENT_big_10,
                                     INCREMENT_big_11))

names(KBS_CZA_BoattiniM_2023)

KBS_CZA_BoattiniM_2023 %>% 
  mutate(DM=as.numeric(DM)) %>%
  mutate(In_hospital_death=ifelse(In_hospital_death==1,"YES", "NO")) %>%
  group_by(In_hospital_death) %>%
  mutate(n=n()) %>%
  mutate(pos=sum(DM))  %>%
  select(In_hospital_death, n, pos) %>% distinct() %>%
  mutate(prop=100*pos/n) %>% ungroup() %>%
  ggplot(aes(In_hospital_death, prop, fill = factor(In_hospital_death))) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.6, size = 0.2, show.legend = F) +
  geom_text(aes(In_hospital_death, prop, label = paste0(round(prop),"%") ), 
            position = position_dodge(width = 0.95), size=5) +
  ggsci::scale_color_jco()+
  ggsci::scale_fill_jco()+
  ylim(0,100) +
  theme_minimal() +
  xlab("\n In-hospital mortality") + ylab("Proportion (%) \n") + ggtitle("DM") +
  theme(plot.title = element_text(face = "bold", size = (10)))



KBS_CZA_BoattiniM_2023 %>% 
  mutate(PITT_score=as.numeric(PITT_score)) %>%
  mutate(D30_mortality=ifelse(D30_mortality==1,"YES", "NO")) %>%
  group_by(D30_mortality) %>%
  ggplot(aes(PITT_score, fill = factor(D30_mortality), colour = factor(D30_mortality))) + 
  geom_density(alpha=0.6, show.legend = T) +
  ggsci::scale_color_jco()+
  ggsci::scale_fill_jco() +
  theme_minimal() +
  xlab("\n PITT score") + ylab("Proportion (%) \n") + ggtitle("PITT score") +
  theme(plot.title = element_text(face = "bold", size = (10)))

# --------------------
# Compare proportions: Fisher's exact vs Mann Withney ------------------------------------

KBS_CZA_BoattiniM_2023 <- fread("KBS_CZA_BoattiniM_2023.txt", sep = "\t")

data.frame(colSums(is.na(KBS_CZA_BoattiniM_2023)))
  
KBS_CZA_BoattiniM_2023$D30_mortality <- as.factor(KBS_CZA_BoattiniM_2023$D30_mortality)
KBS_CZA_BoattiniM_2023$In_hospital_death <- as.factor(KBS_CZA_BoattiniM_2023$In_hospital_death)

KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% mutate(Prolonged_B_lact_infusion = ifelse(is.na(Prolonged_B_lact_infusion), 0, Prolonged_B_lact_infusion))
KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% mutate(KPC_Infection_relapse = ifelse(is.na(KPC_Infection_relapse), 0, KPC_Infection_relapse))
KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% mutate(Dyalisis_30day_preceding = ifelse(is.na(Dyalisis_30day_preceding), 1, Dyalisis_30day_preceding))

data.frame(KBS_CZA_BoattiniM_2023)
KBS_CZA_BoattiniM_2023[, ] <- lapply(KBS_CZA_BoattiniM_2023[ , ], as.factor)
KBS_CZA_BoattiniM_2023[, 1] <- lapply(KBS_CZA_BoattiniM_2023[ , 1], as.numeric)
KBS_CZA_BoattiniM_2023[, 12] <- lapply(KBS_CZA_BoattiniM_2023[ , 12], as.numeric)
KBS_CZA_BoattiniM_2023[, 18] <- lapply(KBS_CZA_BoattiniM_2023[ , 18], as.numeric)
KBS_CZA_BoattiniM_2023[, 19] <- lapply(KBS_CZA_BoattiniM_2023[ , 19], as.numeric)
KBS_CZA_BoattiniM_2023[, 20] <- lapply(KBS_CZA_BoattiniM_2023[ , 20], as.numeric)
KBS_CZA_BoattiniM_2023[, 21] <- lapply(KBS_CZA_BoattiniM_2023[ , 21], as.numeric)
KBS_CZA_BoattiniM_2023[, 35] <- lapply(KBS_CZA_BoattiniM_2023[ , 35], as.numeric)
KBS_CZA_BoattiniM_2023[, 40] <- lapply(KBS_CZA_BoattiniM_2023[ , 40], as.numeric)

#KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023[, -c("TIGE_susceptible")]
#KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023[, -c("MEV_susceptibility")]

KBS_CZA_BoattiniM_2023$D30_mortality[is.na(KBS_CZA_BoattiniM_2023$D30_mortality)] <- 0


names(KBS_CZA_BoattiniM_2023)

temp <- as.matrix(
  KBS_CZA_BoattiniM_2023 %>% group_by(D30_mortality, Combo_three_or_more_antimicr) %>% 
    count() %>% ungroup() %>%
  spread(key=Combo_three_or_more_antimicr, value=n))

matrix(as.numeric(c(temp[1,2], temp[1,3], temp[2,2], temp[2,3])), nrow=2) 

fisher.test( matrix(as.numeric(c(temp[1,2], temp[1,3], temp[2,2], temp[2,3])), nrow=2)  )

# chisq.test(matrix(as.numeric(c(temp[1,2], temp[1,3], temp[2,2], temp[2,3])), nrow=2))


KBS_CZA_BoattiniM_2023 %>% group_by(In_hospital_death) %>% summarise(n=mean(CPE_INCREMENT))

wilcox.test(Charlson_Index~D30_mortality, data = KBS_CZA_BoattiniM_2023)


# -------------------------------------------
# Random Forest - reduced variables for accuracy ----------------------

KBS_CZA_BoattiniM_2023 <- fread("KBS_CZA_BoattiniM_2023.txt", sep = "\t")


data.frame(colSums(KBS_CZA_BoattiniM_2023==""))
  
KBS_CZA_BoattiniM_2023$D30_mortality[KBS_CZA_BoattiniM_2023$D30_mortality==""] <- 0

KBS_CZA_BoattiniM_2023$D30_mortality <- as.factor(KBS_CZA_BoattiniM_2023$D30_mortality)
KBS_CZA_BoattiniM_2023$In_hospital_death <- as.factor(KBS_CZA_BoattiniM_2023$In_hospital_death)

KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% mutate(Prolonged_B_lact_infusion = ifelse(Prolonged_B_lact_infusion=="", 0, Prolonged_B_lact_infusion))
KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% mutate(KPC_Infection_relapse = ifelse(KPC_Infection_relapse=="", 0, KPC_Infection_relapse))
KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% mutate(Dyalisis_30day_preceding = ifelse(Dyalisis_30day_preceding=="", 1, Dyalisis_30day_preceding))

KBS_CZA_BoattiniM_2023[, 1] <- lapply(KBS_CZA_BoattiniM_2023[ , 1], as.numeric)
KBS_CZA_BoattiniM_2023[, 12] <- lapply(KBS_CZA_BoattiniM_2023[ , 12], as.numeric)
KBS_CZA_BoattiniM_2023[, 18] <- lapply(KBS_CZA_BoattiniM_2023[ , 18], as.numeric)
KBS_CZA_BoattiniM_2023[, 19] <- lapply(KBS_CZA_BoattiniM_2023[ , 19], as.numeric)
KBS_CZA_BoattiniM_2023[, 20] <- lapply(KBS_CZA_BoattiniM_2023[ , 20], as.numeric)
KBS_CZA_BoattiniM_2023[, 21] <- lapply(KBS_CZA_BoattiniM_2023[ , 21], as.numeric)
KBS_CZA_BoattiniM_2023[, 35] <- lapply(KBS_CZA_BoattiniM_2023[ , 35], as.numeric)
KBS_CZA_BoattiniM_2023[, 40] <- lapply(KBS_CZA_BoattiniM_2023[ , 40], as.numeric)

KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% mutate_if(is.character, as.factor)
                                     
KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023[, -c("TIGE_susceptible")]
KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023[, -c("MEV_susceptibility")]

KBS_CZA_BoattiniM_2023 %>% group_by(In_hospital_death) %>% summarise(n=mean(PITT_score)) 
KBS_CZA_BoattiniM_2023 %>% group_by(In_hospital_death, COL_susceptible) %>% count()

KBS_CZA_BoattiniM_2023 <- KBS_CZA_BoattiniM_2023 %>% select(-c(Charlson_big3,
                                     Charlsonbig4, 
                                     Charlsonbig5, 
                                     Charlsonbig6,
                                     PITT_scorebig2,
                                     PITT_scorebig3,
                                     PITT_scorebig4,
                                     PITT_scorebig5,
                                     INCREMENT_big_8,
                                     INCREMENT_big_9,
                                     INCREMENT_big_10,
                                     INCREMENT_big_11))

temp <- KBS_CZA_BoattiniM_2023[,-60]

names(temp)

temp <- temp %>% drop_na()

temp$D30_mortality <- as.factor(temp$D30_mortality)

modelAll_1_randomForest <- randomForest(D30_mortality ~    Dyalisis_30day_preceding  + AKI  + CKD  + Cardiosurgery , data = temp, ntress=10000, ntry=2)

print(modelAll_1_randomForest)

summary(modelAll_1_randomForest)

modelAll_1_randomForest$importance


table_mat <- table(temp$D30_mortality,predict(modelAll_1_randomForest, temp))


accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test # 0.8918919


precision <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}


recall <- function(matrix) {
  # true positive
  tp <- matrix[2, 2]# false positive
  fn <- matrix[2, 1]
  return (tp / (tp + fn))
}


prec <- precision(table_mat)
prec # 075

rec <- recall(table_mat)
rec # 075

f1 <- 2 * ((prec * rec) / (prec + rec))
f1 # 0.75


table_mat <- as.data.frame(table_mat)

ggplot(table_mat, aes(x=Var1, y=Var2, fill=Freq)) + geom_tile() +
  theme_minimal() +
 scale_fill_gradient(low = "snow2", high = "steelblue4")



# ------------------------------------------