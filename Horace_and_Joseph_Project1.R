setwd("C:/Users/horac/Desktop/ECO321")

data_scores <- read.table("scores.csv", sep=",", header = TRUE)
data_school_temporary <- read.table("2013_-_2018_Demographic_Snapshot_School.csv", sep=",", header = TRUE, fill = TRUE)

#below are packages for modeling ols.fit under heteroskedastic robust standards
library(sandwich)
library(lmtest)
library(e1071) #package for modelling skewness
library(car) #to test single restrictions on multiple coefficients


#filter out only the 2014-15 years inside the school demographic csv file
filter_out_data_schools<-data_school_temporary[(data_school_temporary$Year=="2014-15"),]

#combining both datasets using the school ID/DBN (b/c some school names are spelled differently between datasets)
data_final_scores_and_schools<- merge(x=data_scores,y=filter_out_data_schools,by.x='School.ID',by.y='DBN')

#getting rid of one of the columns for school names because once merged, 
#there are two columns for names, enrollment, and percentages of race
actual_data_used <- data_final_scores_and_schools[,-c(2, 14, 15, 16, 17, 18)]
attach(actual_data_used)


#################################################################################################################


#basic stats about data used (Average SAT Math score at school)
summary(actual_data_used$"Average.Score..SAT.Math.")
sd(actual_data_used$"Average.Score..SAT.Math.", na.rm=TRUE) #finding standard deviation but ignoring null values
hist(actual_data_used$"Average.Score..SAT.Math.", 
     main = "Average Math SAT Score Histogram", 
     xlab = "Average Math SAT Score",
     freq = TRUE)
skewness(actual_data_used$"Average.Score..SAT.Math.", na.rm=TRUE)
SAT_Math_mean <- mean(actual_data_used$"Average.Score..SAT.Math.", na.rm=TRUE) #finding the mean and setting it to a variable
SAT_Math_var <- var(actual_data_used$"Average.Score..SAT.Math.", na.rm=TRUE) #finding the variance and setting it to a variable
SAT_Math_mean_ci <- SAT_Math_mean + sqrt(SAT_Math_var) * c(-1.96,1.96) #this is the confidence interval at 95%
SAT_Math_pop_ci <- 400 + sqrt(SAT_Math_var) * c(-1.96,1.96) #hypothesis that true population mean is 400

#basic stats about data used (Average SAT Reading score at school)
summary(actual_data_used$"Average.Score..SAT.Reading.")
sd(actual_data_used$"Average.Score..SAT.Reading.", na.rm=TRUE) #finding standard deviation but ignoring null values
hist(actual_data_used$"Average.Score..SAT.Reading.", 
     main = "Average Reading SAT Histogram", 
     xlab = "Average Reading SAT",
     freq = TRUE)
skewness(actual_data_used$"Average.Score..SAT.Reading.", na.rm=TRUE)
SAT_Reading_mean <- mean(actual_data_used$"Average.Score..SAT.Reading.", na.rm=TRUE) #finding the mean and setting it to a variable
SAT_Reading_var <- var(actual_data_used$"Average.Score..SAT.Reading.", na.rm=TRUE) #finding the variance and setting it to a variable
SAT_Reading_mean_ci <- SAT_Reading_mean + sqrt(SAT_Reading_var) * c(-1.96,1.96) #this is the confidence interval at 95%
SAT_Reading_pop_ci <- 450 + sqrt(SAT_Reading_var) * c(-1.96,1.96) #hypothesis that true population mean is 400

#basic stats about data used (Average SAT Writing score at school)
summary(actual_data_used$"Average.Score..SAT.Writing.")
sd(actual_data_used$"Average.Score..SAT.Writing.", na.rm=TRUE) #finding standard deviation but ignoring null values


#basic stats about data used (TotalEnrollment)
summary(actual_data_used$"TotalEnrollment")
#setting the total enrollment as the dummy variable that we are going to use later on for ols estimators
TotalEnroll <- ifelse(actual_data_used$TotalEnrollment < 700,0,1)
summary(TotalEnroll)
sd(TotalEnroll)

#basic stats about data used (Number of English Language Learners in the school)
summary(actual_data_used$"NumEnglishLanguageLearners")
sd(actual_data_used$"NumEnglishLanguageLearners")

#basic stats about data used (Number of students in poverty at school)
summary(actual_data_used$"NumPoverty")
sd(actual_data_used$"NumPoverty")

#basic stats about data used (Number of Asian students in schools)
summary(actual_data_used$"NumAsian")
sd(actual_data_used$"NumAsian")

#basic stats about data used (Number of White students in schools)
summary(actual_data_used$"NumWhite")
sd(actual_data_used$"NumWhite")

#basic stats about data used (Number of Black students in schools)
summary(actual_data_used$"NumBlack")
sd(actual_data_used$"NumBlack")

#basic stats about data used (Number of Hispanic students in schools)
summary(actual_data_used$"NumHispanic")
sd(actual_data_used$"NumHispanic")


#################################################################################################################


#ols data just comparing SAT Math scores with only student enrollment (dummy variable)
plot(TotalEnroll,actual_data_used$Average.Score..SAT.Math.,main='Average SAT Math Score Affected by School Size',xlab='Total Enrollment',ylab='SAT Math Score',type='p',cex=1,col='black')
ols.fit.Math <- lm(Average.Score..SAT.Math. ~ TotalEnroll, data = actual_data_used)
abline(ols.fit.Math, col='red')
summary(ols.fit.Math) #under homoskedasticity
sum.ols.hetero.Math <- coeftest(ols.fit.Math, vcov = vcovHC(ols.fit.Math, "HC1")) #under heteroskedastic robust standard error
tbeta1_SATMath <- ols.fit.Math$coefficients[2]/sum.ols.hetero.Math[[4]] #finding T-test for H0: beta1  = 0, fix alpha = 0.01
abs(tbeta1_SATMath)<qnorm(0.995)
CI_beta1_SATMath <- ols.fit.Math$coefficients[2] + sum.ols.hetero.Math[[4]]*c(-qnorm(0.975),qnorm(0.975)) #confidence interval at 95%


#ols data comparing SAT Math scores with student enrollment (dummy variable), number of students in ELL, number of students in poverty,
#number of Asian students, number of White students, number of Black students, and number of Hispanic students
ols.fit.Math.full <- lm(Average.Score..SAT.Math. ~ TotalEnroll + NumEnglishLanguageLearners +
                          NumPoverty + NumAsian + NumWhite + NumBlack + NumHispanic, data = actual_data_used)
summary(ols.fit.Math.full)
sum.ols.hetero.Math.full <- coeftest(ols.fit.Math.full, vcov = vcovHC(ols.fit.Math.full, "HC1")) #under heteroskedastic robust standard error
waldtest(ols.fit.Math.full, c("NumEnglishLanguageLearners","NumPoverty", "NumAsian", "NumWhite",
                              "NumBlack", "NumHispanic"), vcov = vcovHC(ols.fit.Math.full, "HC1"))
linearHypothesis(ols.fit.Math.full, vcov = vcovHC(ols.fit.Math.full, "HC1"),hypothesis.matrix = c(0,0,1,-1,0,0,0,0), rhs=0)

#################################################################################################################


#ols data just comparing SAT Reading scores with only student enrollment (dummy variable)
plot(TotalEnroll,actual_data_used$Average.Score..SAT.Reading.,main='Average SAT Reading Score Affected by School Size',xlab='Total Enrollment',ylab='SAT Reading Score',type='p',cex=1,col='black')
ols.fit.Reading <- lm(Average.Score..SAT.Reading. ~ TotalEnroll, data = actual_data_used)
abline(ols.fit.Reading, col='red')
summary(ols.fit.Reading) #under homoskedasticity
sum.ols.hetero.Reading <- coeftest(ols.fit.Reading, vcov = vcovHC(ols.fit.Reading, "HC1")) #under heteroskedastic robust standard error
tbeta1_SATReading <- ols.fit.Reading$coefficients[2]/sum.ols.hetero.Reading[[4]] #finding T-test for H0: beta1  = 0, fix alpha = 0.01
abs(tbeta1_SATReading)<qnorm(0.995)
CI_beta1_SATReading <- ols.fit.Reading$coefficients[2] + sum.ols.hetero.Reading[[4]]*c(-qnorm(0.975),qnorm(0.975)) #confidence interval at 95%

#ols data comparing SAT Reading scores with student enrollment (dummy variable), number of students in ELL, number of students in poverty,
#number of Asian students, number of White students, number of Black students, and number of Hispanic students
ols.fit.Reading.full <- lm(Average.Score..SAT.Reading. ~ TotalEnroll + NumEnglishLanguageLearners +
                          NumPoverty + NumAsian + NumWhite + NumBlack + NumHispanic, data = actual_data_used)
summary(ols.fit.Reading.full)
sum.ols.hetero.Reading.full <- coeftest(ols.fit.Reading.full, vcov = vcovHC(ols.fit.Math.full, "HC1")) #under heteroskedastic robust standard error
waldtest(ols.fit.Reading.full, c("NumEnglishLanguageLearners","NumPoverty", "NumAsian", "NumWhite",
                              "NumBlack", "NumHispanic"), vcov = vcovHC(ols.fit.Reading.full, "HC1"))
linearHypothesis(ols.fit.Reading.full, vcov = vcovHC(ols.fit.Reading.full, "HC1"),hypothesis.matrix = c(0,0,1,-1,0,0,0,0), rhs=0)


#################################################################################################################


#ols data just comparing SAT Writing scores with only student enrollment (dummy variable)
plot(TotalEnroll,actual_data_used$Average.Score..SAT.Writing.,main='Average SAT Writing Score Affected by School Size',xlab='Total Enrollment',ylab='SAT Writing Score',type='p',cex=1,col='black')
ols.fit.Writing <- lm(Average.Score..SAT.Writing. ~ TotalEnroll, data = actual_data_used)
abline(ols.fit.Writing, col='red')
summary(ols.fit.Writing)
sum.ols.hetero.Writing <- coeftest(ols.fit.Writing, vcov = vcovHC(ols.fit.Writing, "HC1")) #under heteroskedastic robust standard error
tbeta1_SATWriting <- ols.fit.Writing$coefficients[2]/sum.ols.hetero.Writing[[4]] #finding T-test for H0: beta1  = 0, fix alpha = 0.01
abs(tbeta1_SATWriting)<qnorm(0.995)
CI_beta1_SATWriting <- ols.fit.Writing$coefficients[2] + sum.ols.hetero.Writing[[4]]*c(-qnorm(0.975),qnorm(0.975)) #confidence interval at 95%

#ols data comparing SAT Writing scores with student enrollment (dummy variable), number of students in ELL, number of students in poverty,
#number of Asian students, number of White students, number of Black students, and number of Hispanic students
ols.fit.Writing.full <- lm(Average.Score..SAT.Writing. ~ TotalEnroll + NumEnglishLanguageLearners +
                          NumPoverty + NumAsian + NumWhite + NumBlack + NumHispanic, data = actual_data_used)
summary(ols.fit.Writing.full)
sum.ols.hetero.Writing.full <- coeftest(ols.fit.Writing.full, vcov = vcovHC(ols.fit.Writing.full, "HC1")) #under heteroskedastic robust standard error
waldtest(ols.fit.Writing.full, c("NumEnglishLanguageLearners","NumPoverty", "NumAsian", "NumWhite",
                              "NumBlack", "NumHispanic"), vcov = vcovHC(ols.fit.Writing.full, "HC1"))
linearHypothesis(ols.fit.Writing.full, vcov = vcovHC(ols.fit.Writing.full, "HC1"),hypothesis.matrix = c(0,0,1,-1,0,0,0,0), rhs=0)


