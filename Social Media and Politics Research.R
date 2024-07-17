##Social Media and Politics Research##

rm(list=ls(all=TRUE))

#load packages
library(class)
library(rpart)
library(caret)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(skimr)
library(readxl)
library(car)

install.packages("modelsummary")
library(modelsummary)
install.packages("stargazer")
library(stargazer)

#import dataset
smpolitics.df <- read_xlsx("Desktop/Masters/Dissertation/Social Media and Political Movements.xlsx")
view(smpolitics.df)


##methodology: descriptive model##

#baseline
#logistic regression (descriptive model so full dataset used)
reg <- glm(Participation ~ Exposure, data =smpolitics.df,family= 'binomial')
summary(reg)

modelsummary(reg, stars= TRUE)
stargazer(reg, type = "text")
stargazer(reg, type = "text", style = "asr")
tab_model(reg)


#addition of controls
reg_2 <- glm(Participation ~ Exposure + Age
             + Gender + Nationality + Ethnicity
             + Area_of_Residence + Country_of_Residence
             + Same_Country + Education + Employment_Status
             + Sector + Occupation_Level + Political_Ideology 
             + Social_Media_Consumption + Other_Media_Consumption, 
             data =smpolitics.df,family= 'binomial')
summary(reg_2)
#nothing significant
#vif_2 <- vif(reg_2) #aliased coefficients


reg_3 <-  glm(Participation ~ Exposure + Age
              + Gender + Nationality
              + Area_of_Residence + Country_of_Residence
              + Same_Country + Education + Employment_Status
              + Sector + Occupation_Level + Political_Ideology 
              + Social_Media_Consumption + Other_Media_Consumption, 
              data =smpolitics.df,family= 'binomial')
summary(reg_3)
#nothing significant
#vif_3 <- vif(reg_3) #aliased coefficients

reg_4 <-  glm(Participation ~ Exposure + Age
              + Gender + Nationality
              + Area_of_Residence + Country_of_Residence
              + Same_Country + Education
              + Sector + Occupation_Level + Political_Ideology 
              + Social_Media_Consumption + Other_Media_Consumption, 
              data =smpolitics.df,family= 'binomial')
summary(reg_4)
#vif_4 <- vif(reg_4) #aliased coefficients

reg_5 <-  glm(Participation ~ Exposure + Age
              + Gender + Nationality 
              + Area_of_Residence + Country_of_Residence
              + Same_Country + Education
             + Occupation_Level + Political_Ideology 
              + Social_Media_Consumption + Other_Media_Consumption, 
              data =smpolitics.df,family= 'binomial')
summary(reg_5)
#vif_5 <- vif(reg_5) #aliased coefficients

reg_6 <-  glm(Participation ~ Exposure + Age
              + Gender + Nationality + Area_of_Residence
              + Same_Country + Education
              + Occupation_Level + Political_Ideology 
              + Social_Media_Consumption + Other_Media_Consumption, 
              data =smpolitics.df,family= 'binomial')
summary(reg_6)
vif_6 <- vif(reg_6)

reg_7 <-  glm(Participation ~ Exposure + Age
              + Gender + Area_of_Residence
              + Same_Country + Education + Political_Ideology 
              + Social_Media_Consumption + Other_Media_Consumption, 
              data =smpolitics.df,family= 'binomial')
summary(reg_7)
vif_7 <- vif(reg_7)

reg_8 <- glm(Participation ~ Exposure + Age
             + Gender + Area_of_Residence
             + Same_Country + Political_Ideology 
             + Social_Media_Consumption + Other_Media_Consumption, 
             data =smpolitics.df,family= 'binomial')
summary(reg_8)
vif_8 <- vif(reg_8)

reg_9 <-  glm(Participation ~ Exposure + Age
              + Gender + Area_of_Residence
              + Same_Country + Political_Ideology 
              + Social_Media_Consumption, 
              data =smpolitics.df,family= 'binomial')
summary(reg_9)
vif_9 <- vif(reg_9)

reg_10 <-  glm(Participation ~ Exposure + Age
              + Gender + Area_of_Residence
              + Same_Country 
              + Social_Media_Consumption, 
              data =smpolitics.df,family= 'binomial')
summary(reg_10)
vif_10 <- vif(reg_10)

reg_11 <- glm(Participation ~ Exposure
             + Gender + Area_of_Residence
             + Same_Country + Political_Ideology
             + Social_Media_Consumption, 
             data =smpolitics.df,family= 'binomial')
summary(reg_11)
vif_11 <- vif(reg_11)

reg_12 <- glm(Participation ~ Exposure
              + Area_of_Residence
              + Same_Country + Political_Ideology
              + Social_Media_Consumption, 
              data =smpolitics.df,family= 'binomial')
summary(reg_12) 
vif_12 <- vif(reg_12)

#choice between reg_8 reg_9 and reg_11
modelsummary(list(reg_8, reg_9, reg_11), stars= TRUE)
stargazer(list(reg_8, reg_10), type = 'text')

McFadden_R2_reg8 <- 1- (144.42/186.64)
print(McFadden_R2_reg8)

McFadden_R2_reg9 <- 1- (145.25/186.64)
print(McFadden_R2_reg9)

McFadden_R2_reg11 <- 1- (152.09/186.64)
print(McFadden_R2_reg11)

stargazer(reg_8, reg_9, reg_11, type= "text")


##best model##
sm_politics_reg <- glm(Participation ~ Exposure + Age + Gender + 
                         Area_of_Residence + Same_Country + Political_Ideology + 
                         Social_Media_Consumption, data =smpolitics.df,family= 'binomial')
summary(sm_politics_reg)
vif_smpoliticsreg <- vif(sm_politics_reg)
print(vif_smpoliticsreg)

modelsummary(list(reg, sm_politics_reg),
             stars= TRUE)
modelsummary(sm_politics_reg, stars= TRUE)
stargazer(sm_politics_reg, type = "text", style = "asr")


vif_results <- list(vif_6, vif_7, vif_8, vif_9, vif_10, vif_11, vif_12, vif_smpoliticsreg)
print(vif_results)


#summary of all models
all <- list(reg, reg_2, reg_3, reg_4, reg_5, reg_6, reg_7, reg_8, reg_9, reg_10, reg_11, reg_12, sm_politics_reg)
regressions_1 <- list(reg, reg_2, reg_3, reg_4, reg_5, reg_6)
regressions_2 <- list(reg_7, reg_8, reg_9, reg_10,reg_11, reg_12)
modelsummary(regressions_1, stars= TRUE)
modelsummary(regressions_2, stars= TRUE)
modelsummary(all, stars = TRUE)

stargazer(regressions_1, type= "text")
stargazer(regressions_2, type = "text")
stargazer(all, type = "text", style = "asr")


##heterogeneity of effect across different political issues##

#migration
migration_reg <- glm(Participation ~ Migration + Age + Gender + 
                       Area_of_Residence + Same_Country + Political_Ideology + 
                       Social_Media_Consumption, data = smpolitics.df, family = 'binomial')
summary(migration_reg)

#Israeli-Palestinian conflict
middle_east_reg <- glm(Participation ~ Israeli_Palestinian_Conflict + Age + Gender + 
                         Area_of_Residence + Same_Country + Political_Ideology 
                       + Social_Media_Consumption, data = smpolitics.df, family = 'binomial')
summary(middle_east_reg)

#housing crisis
housing_reg <- glm(Participation ~ Housing_Crisis + Age + Gender + 
                     Area_of_Residence + Same_Country + Political_Ideology + 
                     Social_Media_Consumption, data = smpolitics.df, family = 'binomial')
summary(housing_reg)

#climate change
climate_reg <- glm(Participation ~ Climate_Change + Age + Gender + 
                     Area_of_Residence + Same_Country + Political_Ideology + 
                     Social_Media_Consumption, data = smpolitics.df, family = 'binomial')
summary(climate_reg)

#cost of living
cost_reg <- glm(Participation ~ Cost_of_Living + Age + Gender + 
                  Area_of_Residence + Same_Country + Political_Ideology + 
                  Social_Media_Consumption, data = smpolitics.df, family = 'binomial')
summary(cost_reg)

#black lives matter
blm_reg <- glm(Participation ~ BLM + Age + Gender + 
                 Area_of_Residence + Same_Country + Political_Ideology + 
                 Social_Media_Consumption, data = smpolitics.df, family = 'binomial')
summary(blm_reg)


#summary of all models
political_issues <- list(migration_reg, middle_east_reg, housing_reg,
                         climate_reg, cost_reg, blm_reg)
modelsummary(political_issues,
             stars= TRUE)

stargazer(political_issues, type = "text", style = "asr")


##predictive model##

#create training and testing datasets
set.seed(12)
train.index<-sample(c(1:dim(smpolitics.df)[1]),dim(smpolitics.df)[1]*0.7)
valid.index<-setdiff(c(1:dim(smpolitics.df)[1]),train.index)
length(train.index)
length(valid.index)

train.df<-smpolitics.df[train.index,]
valid.df<-smpolitics.df[valid.index,]

head(train.df)
head(valid.df)

#logistic regression (baseline)
predictive_reg<-glm(Participation ~ Exposure + Age + Gender +
         Area_of_Residence + Same_Country + Political_Ideology + 
         Social_Media_Consumption, data =train.df,family= 'binomial')
summary(predictive_reg)
#logistic regression trained on training data

#predicted probabilities of participation for validation data
predicted <- predict(predictive_reg, valid.df, type = 'response')
#convert probabilities to classes
ifelse(predict(predictive_reg, valid.df, type= 'response') > 0.5, 1, 0)

valid.df$Participation <- factor(valid.df$Participation, levels = c(1,0))
predicted_labels <- factor(ifelse(predict(predictive_reg, valid.df, type='response') > 0.5, 1, 0), 
                           levels = levels(valid.df$Participation))

#confusion matrix
confusionMatrix(valid.df$Participation, 
                predicted_labels, 
                dnn = c("Actual", "Predicted"))


#verify findings of the confusion matrix
confusion_table <- table(valid.df$Participation, predicted_labels)
rownames(confusion_table) <- c("Actual: Participated", "Actual: Not Participated")
colnames(confusion_table) <- c("Prediction: Participated", "Prediction: Not Participated")
confusion_table

accuracy <- (53+2)/(2+4+8+53)
accuracy

precision <- 53/(53+8)
precision

recall <- 53/(53+4)
recall

F1_score <- 2*(precision*recall)/(precision+recall)
F1_score




