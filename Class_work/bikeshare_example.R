
# STP 530 midterm review example: bike sharing

rm(list=ls())

#library(psych)
library(Hmisc)

setwd("~/Dropbox (ASU)/ASU/Course/STP530/Slides/midterm_review/bike sharing example/bike sharing R")

# Import the data
mydata <- read.csv("day.csv")

# Inspect the data
head(mydata)
str(mydata)
Hmisc::describe(mydata)

# Column 16 is the response variable. Columns 10-13 are numerical variables that
# can potentially serve as predictors. View the scatterplot matrix of those variables.
pairs(mydata[, c(16, 10:13)])
cor(mydata[, c(16, 10:13)])

# Based on the information of the variables from the dataset website and the
# bivariate relationships, we choose atemp, hum and windspeed as the predictors.
# Fit the multiple regression model.
m1 <- lm(cnt ~ atemp + windspeed + hum, data=mydata)
summary(m1)

# Dignostics of m1
plot(predict(m1), residuals(m1))
plot(mydata$atemp, residuals(m1))
plot(mydata$windspeed, residuals(m1))
plot(mydata$hum, residuals(m1))

# Looks like a quadratic term of atemp should be added
mydata$atemp2 <- mydata$atemp ^ 2
m2 <- lm(cnt ~ atemp + atemp2 + windspeed + hum, data=mydata)
summary(m2)

# Dignostics of m2
# Check for linear fit, which looks okay now.
plot(predict(m2), residuals(m2))
plot(mydata$atemp, residuals(m2))
plot(mydata$windspeed, residuals(m2))
plot(mydata$hum, residuals(m2))

# Check for normal distribution of the residuals. Mostly looking good but see 2
# points sticking out there.
qqnorm(residuals(m2))
qqline(residuals(m2))

# See the studentized residuals. There are indeed 2 regression outliers.
plot(predict(m2), rstudent(m2))

# Identify the two regression outliers and learn more about these two cases.
which(residuals(m2) == max(residuals(m2)))
which(residuals(m2) == min(residuals(m2)))
mydata[c(595, 69), ]

# Remove the 2 outliers
reduced.data <- mydata[-c(595, 69), ]

m3 <- lm(cnt ~ atemp + atemp2 + windspeed + hum, data=reduced.data)
summary(m3)

# Diagnostics of m3
plot(predict(m3), residuals(m3))
plot(predict(m3), rstudent(m3))
qqnorm(residuals(m3))
qqline(residuals(m3))

# m3's diagnostics are not perfect but looks okay. Proceed with interpreteations and inferences.

# Fitted model and interpreting the coefficients
summary(m3)

# Construct a 90% CI of the slope of humidity
summary(m3)

b4 <- -4428.2
se.b4 <- 361.0
t.star <- qt(p=.95, df=724) # p = 1 - alpha/2, df = n - p
LL <- b4 - t.star * se.b4
UL <- b4 + t.star * se.b4
LL; UL

confint(m3, level=.90) # verify with R calculation. Difference caused by rounding error.

# Construct and interpret a 95% confidence interval and prediction interval of 
# the predicted outcome when atemp = .5, hum = .5, windspeed = .3

predict(m, newdata=data.frame(atemp = .5, atemp2 = .25, hum = .5, windspeed = .3), 
        interval="confidence", level=.95)

predict(m, newdata=data.frame(atemp = .5, atemp2 = .25, hum = .5, windspeed = .3), 
        interval="predict", level=.95)

# F-test of global model utility
summary(m3)

# F-test of extra sum of squares of humidity
anova(m3)

# The linear testing approach: whether humidity should be kept in the model when
# when they other terms are in.

full.model <- lm(cnt ~ atemp + atemp2 + windspeed + hum, data=reduced.data)
reduced.model <- lm(cnt ~ atemp + atemp2 + windspeed, data=reduced.data)
anova(reduced.model, full.model)

# Effect size for the model comparison: compare R2's
summary(full.model)
summary(reduced.model)

# Effect size for the model comparison: partial R2
anova(full.model)
anova(reduced.model)

partial.R2 <- (1437668845 - 1190260914) / 1437668845
partial.R2

