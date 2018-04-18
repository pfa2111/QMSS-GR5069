# Data challenge 3

rm(list=ls())     
setwd("/Users/pabloargote/Dropbox/Columbia/02. Semester/Data_Science/Data challenge 3")
violence <- read.csv("AllViolenceData_171220.csv")
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
library(interplot)
library(sjPlot)
library(sjmisc)

set.seed(1183)

# a) Estimating OLS 1

violence$source1 <- factor(violence$source)

summary(lm(detained ~ army*total_people_dead + small_arms_seized + federal_police + cartridge_sezied + clips_seized  
           + vehicles_seized + afi + long_guns_seized + ministerial_police + municipal_police + navy + state_police
           + perfect_lethality,  data = violence))

ols1 <- (lm(detained ~ army*total_people_dead + small_arms_seized + federal_police + cartridge_sezied + clips_seized  
           + vehicles_seized + afi + long_guns_seized + ministerial_police + municipal_police + navy + state_police
           + perfect_lethality,  data = violence))

plot_model(ols1, type = "eff", terms = c("total_people_dead", "army"))



# b) Estimating OLS 2

summary(lm(detained ~ army*source1*federal_police + small_arms_seized  + cartridge_sezied + clips_seized  
           + vehicles_seized + afi + long_guns_seized + ministerial_police + municipal_police + navy + state_police
           + perfect_lethality,  data = violence))

ols2 <- (lm(detained ~ army*total_people_dead + small_arms_seized + federal_police + cartridge_sezied + clips_seized  
            + vehicles_seized + afi + long_guns_seized + ministerial_police + municipal_police + navy + state_police
            + perfect_lethality,  data = violence))

p <- plot_model(ols2, type = "eff", terms = c("total_people_dead", "federal_police", "army"))
p + labs(caption = "Left panel: army not involved. Right panel: army involved")
