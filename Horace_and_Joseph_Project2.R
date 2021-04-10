setwd("C:/Users/horac/Desktop/ECO321")

data_scores <- read.table("scores.csv", sep=",", header = TRUE)
data_school_temporary <- read.table("2013_-_2018_Demographic_Snapshot_School.csv", sep=",", header = TRUE, fill = TRUE)

#below are packages for modeling ols.fit under heteroskedastic robust standards
library(sandwich)
library(lmtest)
library(dplyr)
library(car) #to test single restrictions on multiple coefficients
library(ggplot2)


#filter out only the 2014-15 years inside the school demographic csv file
filter_out_data_schools<-data_school_temporary[(data_school_temporary$Year=="2014-15"),]

#combining both datasets using the school ID/DBN (b/c some school names are spelled differently between datasets)
data_final_scores_and_schools<- merge(x=data_scores,y=filter_out_data_schools,by.x='School.ID',by.y='DBN')

#getting rid of one of the columns for school names because once merged, 
#there are two columns for names, enrollment, and percentages of race
actual_data_used <- data_final_scores_and_schools[,-c(2, 14, 15, 16, 17, 18)]

actual_data_used$SATScore <- c(rowSums(actual_data_used[,c(
  "Average.Score..SAT.Math.", "Average.Score..SAT.Reading.", "Average.Score..SAT.Writing.")]))
actual_data_used <- actual_data_used[,-c(13, 14, 15)]
attach(actual_data_used)

#######################################################################################################################
#setting the total enrollment as the dummy variable that we are going to use later on for ols estimators
TotalEnroll <- ifelse(actual_data_used$TotalEnrollment < 700,0,1)

#adding this line below because log(0) is undefined
actual_data_used$NumEnglishLanguageLearners[which(actual_data_used$NumEnglishLanguageLearners == 0)] <- 1

#remove 55 rows from each plot (110 total) because they don't actually contain SAT Scores (some schools didn't share their scores)
ggplot(actual_data_used, aes(NumEnglishLanguageLearners, SATScore)) + geom_point() + 
  geom_smooth(method = "lm", formula = y~x, se=FALSE) +
  geom_smooth(method = "lm", formula = y~x + I(log(x)), se=FALSE, colour = "red") +
  xlim(0, 150)

#removed 37 rows from each plot (74 total) because they had missing values
ggplot(actual_data_used, aes(NumPoverty, SATScore)) + geom_point() + 
  geom_smooth(method = "lm", formula = y~x, se=FALSE) +
  geom_smooth(method = "lm", formula = y~x + I(x^2), se=FALSE, colour = "red") +
  xlim(0, 1800)

#initial linear model
ols.fit.full <- lm(SATScore ~ TotalEnroll + NumEnglishLanguageLearners +
                          NumPoverty + NumAsian + NumWhite + NumBlack + NumHispanic, data = actual_data_used)
summary(ols.fit.full)
ols.fit.hetero <- coeftest(ols.fit.full, vcov = vcovHC(ols.fit.full, "HC1")) #under heteroskedastic robust standard error

#nonlinear model without the interaction term
ols.fit.log <- lm(SATScore ~ TotalEnroll + NumEnglishLanguageLearners + I(log(NumEnglishLanguageLearners)) + NumPoverty + I(NumPoverty^2) +
                     + NumAsian + NumWhite + NumBlack + NumHispanic, data = actual_data_used)
summary(ols.fit.log)
ols.fit.log.hetero <- coeftest(ols.fit.log, vcov = vcovHC(ols.fit.log, "HC1")) #under heteroskedastic robust standard error

#nonlinear model with the interaction term
ols.fit.log1 <- lm(SATScore ~ TotalEnroll + NumEnglishLanguageLearners + I(log(NumEnglishLanguageLearners)) + NumPoverty + I(NumPoverty^2) +
                     + NumAsian + NumWhite + NumBlack + NumHispanic + I(NumEnglishLanguageLearners*NumPoverty), data = actual_data_used)
summary(ols.fit.log1)
ols.fit.log1.hetero <- coeftest(ols.fit.log1, vcov = vcovHC(ols.fit.log1, "HC1")) #under heteroskedastic robust standard error

#F-test testing if the nonlinear and interaction terms are significant or not
waldtest(ols.fit.log1, c("I(log(NumEnglishLanguageLearners))", "I(NumPoverty^2)", "I(NumEnglishLanguageLearners * NumPoverty)"),
         vcov = vcovHC(ols.fit.log1, "HC1"))

