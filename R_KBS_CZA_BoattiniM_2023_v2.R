library(tidyverse)
library(data.table)
library(broom)
library(randomForest)
library(DALEX)
options(scipen = 999)


# Random Forest - Explanatory Modeling -----------------------------------------------------------------------------------------

KBS_CZA_BoattiniM_2023 <- fread("KBS_CZA_BoattiniM_2023.txt", sep = "\t", colClasses = "character")

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

# -----------------------------------------------------------------------------


# In hospital mortality -----------------------------------------------------------------------------------------


temp <- KBS_CZA_BoattiniM_2023[,-61]

names(temp)

sum(temp=="") # 0
names(temp)[25] <- "Ren_repl_ther_30day_preceding"

modelAll_1_randomForest <- randomForest(In_hospital_death ~ . , data = temp)

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
names(temp)[25] <- "Ren_repl_ther_30day_preceding"

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

# -------------------------------------------------------------
# Random Forest - reduced variables ----------------------

KBS_CZA_BoattiniM_2023 <- fread("KBS_CZA_BoattiniM_2023.txt", sep = "\t" , colClasses = "character")


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

temp <- KBS_CZA_BoattiniM_2023[,-61]

names(temp)

temp <- temp %>% drop_na()

temp$In_hospital_death <- as.factor(temp$In_hospital_death)

modelAll_1_randomForest <- randomForest(In_hospital_death ~    Dyalisis_30day_preceding  + AKI  + CKD  + Cardiosurgery , data = temp, ntress=10000, ntry=2)

print(modelAll_1_randomForest)

summary(modelAll_1_randomForest)

modelAll_1_randomForest$importance


table_mat <- table(temp$In_hospital_death,predict(modelAll_1_randomForest, temp))


accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

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
prec

rec <- recall(table_mat)
rec 

f1 <- 2 * ((prec * rec) / (prec + rec))
f1 

table_mat <- as.data.frame(table_mat)

ggplot(table_mat, aes(x=Var1, y=Var2, fill=Freq)) + geom_tile() +
  theme_minimal() +
 scale_fill_gradient(low = "snow2", high = "steelblue4")

data.frame(temp$In_hospital_death) %>% bind_cols(predict(modelAll_1_randomForest, temp, type = 'prob')) %>%
  arrange(temp.In_hospital_death) %>% 
  mutate(temp.In_hospital_death=as.factor(temp.In_hospital_death)) %>%
  mutate(temp.In_hospital_death=ifelse(temp.In_hospital_death==1,"Dead", "Alive")) %>%
  group_by(temp.In_hospital_death) %>%
  ggplot((aes(`1`, colour=temp.In_hospital_death, fill=temp.In_hospital_death))) +
  geom_density(alpha=0.7) +
  scale_fill_manual(values = c("azure4", "brown3")) +
  scale_colour_manual(values = c("azure4", "brown3")) +
  theme_minimal() +
  xlim(0,1) +
  facet_grid(temp.In_hospital_death~., scales="free_y") +
  xlab("\n Predicted Probability / Propensity Score") +
  ylab("Patient (kernel) density \n")




# --------------------------------------------------------------------------------------






# Plot Summary distributions -------------------------------------------------
# Plot some distribution


KBS_CZA_BoattiniM_2023 <- fread("KBS_CZA_BoattiniM_2023.txt", sep = "\t" , colClasses = "character")


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
  mutate(Charlson_Index=as.numeric(Charlson_Index)) %>%
  mutate(D30_mortality=ifelse(D30_mortality==1,"YES", "NO")) %>%
  group_by(D30_mortality) %>%
  ggplot(aes(Charlson_Index, fill = factor(D30_mortality), colour = factor(D30_mortality))) + 
  geom_density(alpha=0.7, show.legend = T) +
  scale_fill_manual(values = c("steelblue4", "brown3")) +
  scale_colour_manual(values = c("steelblue4", "brown3")) +
  theme_minimal() +
  xlab("\n Charlson Comorbidity Index") + ylab("Patient (kernel) density \n") + ggtitle("Charlson Comorbidity Index") +
  theme(plot.title = element_text(face = "bold", size = (10)))

# ------------------
# Compare proportions: Fisher's exact vs Mann Withney ------------------------------------


KBS_CZA_BoattiniM_2023 <- fread("KBS_CZA_BoattiniM_2023.txt", sep = "\t" , colClasses = "character")


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
                                     
names(KBS_CZA_BoattiniM_2023)

temp <- as.matrix(
  KBS_CZA_BoattiniM_2023 %>% group_by(D30_mortality, Combo_three_or_more_antimicr) %>% 
    count() %>% ungroup() %>%
  spread(key=Combo_three_or_more_antimicr, value=n))

matrix(as.numeric(c(temp[1,2], temp[1,3], temp[2,2], temp[2,3])), nrow=2) 

fisher.test( matrix(as.numeric(c(temp[1,2], temp[1,3], temp[2,2], temp[2,3])), nrow=2)  )

# chisq.test(matrix(as.numeric(c(temp[1,2], temp[1,3], temp[2,2], temp[2,3])), nrow=2))


KBS_CZA_BoattiniM_2023 %>% group_by(In_hospital_death) %>% summarise(n=median(Age))

wilcox.test(CPE_INCREMENT~D30_mortality, data = KBS_CZA_BoattiniM_2023)


