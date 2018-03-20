# Data challenge 2

rm(list=ls())     
setwd("/Users/pabloargote/Dropbox/Columbia/02. Semester/Data_Science/Data challenge 2")
violence_data <- read.csv("AllViolenceData_171220.csv")
library(data.table)
library("margins")
options("scipen" = 5)
library(popbio)
library(bestglm)
library(leaps)
library(caret)
library(lattice)
library(ggplot2)
library(e1071)

# a) Generating my dependent variable

violence_data$civ_violence <- ifelse(violence_data$civilian_dead > 0 | violence_data$civilian_wounded > 0, 1, 0)
table(violence_data$civ_violence)


# b) Estimating logistic regression model 

summary(lm(civ_violence ~  detained + long_guns_seized + small_arms_seized + cartridge_sezied + clips_seized  
            + vehicles_seized + afi + army + federal_police + ministerial_police + municipal_police + navy + state_police
            + perfect_lethality,  data = violence_data))

logit1 <- glm(civ_violence ~  detained + long_guns_seized + small_arms_seized + cartridge_sezied + clips_seized  
+ vehicles_seized + afi + army + federal_police + ministerial_police + municipal_police + navy + state_police
+ perfect_lethality, family = binomial(link = "logit"), data = violence_data)
margins(logit1)
margins(logit1,at = list(army = 0))

# b) Visualization

logi.hist.plot(violence_data$civ_violence ,violence_data$army, logi.mod = 1, boxp=FALSE,type="hist",col="gray",
               ylabel="Violence against civilians probability", 
               xlab="Army involvement" )   

# c) Cross-validation

train_control <- trainControl(method="repeatedcv", number=10, repeats = 10)
violence_data$civ_violence <- as.factor(violence_data$civ_violence )


# Model 1: same than before

logit_cross1 <- train(civ_violence ~  detained + long_guns_seized + small_arms_seized + cartridge_sezied + clips_seized  
                     + vehicles_seized + afi + army + federal_police + ministerial_police + municipal_police + navy + state_police
                     + perfect_lethality, data = violence_data, trControl=train_control, method="glm", 
                     family="binomial")
print(logit_cross1)
varImp(object=logit_cross1)
plot(varImp(object=logit_cross1),main="GLM - Variable Importance")


# Model 2: only significant predidctors from previous section

logit_cross2 <- train(civ_violence ~  long_guns_seized + cartridge_sezied + army + federal_police  + municipal_police + navy + 
                      perfect_lethality, data = violence_data, trControl=train_control, method="glm", 
                      family="binomial")

print(logit_cross2)
varImp(object=logit_cross2)
plot(varImp(object=logit_cross2),main="GLM - Variable Importance")

# Model 3: only significant predidctors from previous section at the 0.1 level + interactions

logit_cross3 <- train(civ_violence ~  long_guns_seized + cartridge_sezied + army + federal_police  + municipal_police + navy + 
                        perfect_lethality + army*navy + army*long_guns_seized + army*cartridge_sezied + army*federal_police +
                      army*municipal_police + army*perfect_lethality, data = violence_data, trControl=train_control, method="glm", 
                      family="binomial")

print(logit_cross3)
varImp(object=logit_cross3)
plot(varImp(object=logit_cross3),main="GLM - Variable Importance")


