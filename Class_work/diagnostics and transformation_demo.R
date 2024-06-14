
# STP 530 Demo of diagnostics and transformation

rm(list=ls()) # It's a good practice to clear working space before starting a new data

#-------------------------------------------------
# install packages

# If you haven't installed some of the packages below, you can un-comment those 
# lines and install the packages. You only need to install a package once on your laptop
# (unless there is a recent update you want to install.)

#install.packages("faraway")
#install.packages("car")
#install.pacakges("Hmisc")
#install.packages("psych")

# load packages

# To use a package, you need to load it. Every time you open a new R session, you
# need to load all packages you'd like to use in the code.

library(faraway)
library(car)

library(Hmisc)
# Because the describe() function in Hmisc and psych packages share the same name,
# loading the psych package after loading the Hmisc package will overwrite the 
# describe() function provided by Hmisc. To make both functions available to you, 
# you can rename them like below.
H.describe <- describe  

library(psych)
P.describe <- describe

#-------------------------------------------------
# load and examine the data

# Load the data provided by a package by data()
data(airquality)

# Take a quick look at the dataset. Note there are NA's in the dataset.
head(airquality) 

# Use the following code to view the manual of the data provided by the package
?airquality

# Learn more about the structure of the data
str(airquality)

#-------------------------------------------------
# Inspect each variable: know what cards you have in your hand.

# The following are three handy functions you can choose from to get a numerical 
# summary of each variable in a dataset

summary(airquality)
H.describe(airquality)
P.describe(airquality)

# Examine the histogram of each variable to check the distribution and outliers,
# be on the look for potential data entry errors.

hist(airquality$Ozone)
hist(airquality$Solar.R)
hist(airquality$Wind)
hist(airquality$Temp)

#-------------------------------------------------
# Visualize bivariate relationships

# pairs() generates a scatterplot matrix.

# Inspect the first row (or the row involving Y) to learn about how Y relates to
# each X (marginally). Be on the look for non-linear trend.

# The remainder of the scatterplot matrix (relationship among the Xs themselves)
# can inform about the multicollinearity issue, which will be explained later.

pairs(airquality[ , 1:4])

#-------------------------------------------------
# Fit a multiple regression model.

# Note: Use "na.action=na.exclude" to avoid lurking errors in R caused by NAs in the data.
# If "na.action=na.exclude" is not specified, the default way lm() handles missing
# data will result in outputs from predict(), fitted(), residuals(), etc. having
# a different length than the original data -- all rows with missing data are removed.
# Setting "na.action=na.exclude" asks R to pad the outputs from predict(), fitted(), 
# residuals(), etc. with NA for those rows with missing data, so all the above 
# outputs have the right correspondence. 

mod <- lm(Ozone ~ Solar.R + Wind + Temp, data=airquality, na.action=na.exclude)

# You may take a look at the model results now, but keep in mind that until all 
# diagnostic items below are passed, you should not interpret any of the hypothesis
# test results (e.g., t-test, F-test).

summary(mod)

################################################################################
# (1) Verify the regression function is linear
################################################################################

# Plot the residuals against the fitted values (Y-hat) to spot any non-linear 
# relationship bewteen Y and the X's that has not been captured by the fitted 
# first-order linear model

plot(fitted(mod), residuals(mod))
# Impression: there seems to be a U-shaped non-linear trend, but the upward
# tail on the left hand side has much fewer data points. The biggest problem
# seems to be heteroskedasticity (see #3 below).

# You can also plot the residuals against each X to see whether the nonlinear trend
# in the residuals might be potentially related to one of the Xs.

plot(airquality$Solar.R, residuals(mod))
plot(airquality$Wind, residuals(mod))
plot(airquality$Temp, residuals(mod))
# Impression: there seems to be a vague U-shape relationship between the residuals
# and Temperature. Consider adding a quadratic term for Temp? Later you'll learn
# that to determine whether an additional quadratic term is worth it, we need to
# rely on a valid hypothesis test (which compares the two candidate models). 
# And we need to fix the heteroskedasticity first to render a valid hypothesis test.

################################################################################
# (2) Spot any regression outliers
################################################################################

# An outlier in a regression model is a data point with an excessive residual value.
# A data point with an extreme value for X or Y is not necessarily an outlier for 
# the regression model if the point falls close to the regression line.

# Because the original residual can come in any unit and scale, their magnitudes
# can not be interpreted with the same standard. We need to standardize the residuals
# to put them on the same scale. Use the rstudent() function to obtain the studentized
# residuals, which follow a t-distribution, which roughly resembles a N(0, 1)
# distribution. So absolute values beyond 2 or 3 can be considered excessive.

# Studentized residual plot
plot(fitted(mod), rstudent(mod))

# The following code helps you find out which point is the outlier
plot(fitted(mod), rstudent(mod), type="n")
text(fitted(mod), rstudent(mod), names(rstudent(mod)))
# Impression: data point 117 seems to have an excessive positive residual.
# But, again, let's fix the heteroskedasticity first.

################################################################################
# (3) Check the residuals to make sure they have constant 
# variance (vertical spread) across the board (homoskedasticity)
################################################################################

# We can look at same residual plot as in (1) or (2): Plot the residuals (or the
# studentized residuals) against the fitted values (Y-hat).

plot(fitted(mod), residuals(mod))
# This funnel shape that opens to the right is perhaps the most commonly seen
# heteroskedasticity pattern, which is often related to a frequency count type of 
# Y that follows a Poisson distribution. A log transformation of Y (see below)
# is the typical solution to this problem. See bottom of this file for more discussion
# about the Box-Cox transformation.

################################################################################
# (4) Check the residuals to make sure they are normally distributed
################################################################################

# You can look at the histogram of the residuals to see if it's bell-shaped

hist(residuals(mod))

# The more precise way: the Q-Q plot. The straight reference line represents a 
# perfect normal distribution. 

qqnorm(residuals(mod))
qqline(residuals(mod))
# Impression: There is mild to moderate deviation from the normal distribution.
# The histogram also indicates that the distribution is a bit skewed.
# A transformation of Y usually also changes the residual distribution. Since
# we will transform Y to fix #3 above. Let's come back after the transformation.

# In general, t-tests are robust to mild to moderate violoation of the normal 
# assumption. So this level of deviation from the normal distribution is usually
# fine.

################################################################################
# (5) Make sure the error terms are independent
################################################################################

# This can only be determined based on how the data are collected, not by
# looking at the data themselves, except for time-series data, where you can 
# plot your data against time to gauge the direction and magnitude of autocorrelation.


#-------------------------------------------------
# Transform Y with log(), fit the new model, and repeat the diagnostics

airquality$log.Ozone <- log(airquality$Ozone)

logmod <- lm(log.Ozone ~ Solar.R + Wind + Temp, data=airquality, na.action=na.exclude)
summary(logmod)

# Residual plots of the log-transformed model

plot(fitted(logmod), rstudent(logmod))
# Impression: Most of the heteroskedasticity has been fix! 
# There seems to one outlier down there, with a studentized residual of <-4, which is definitely excessive. 
# In addition, there seems to be less of a U-shape curvature as well. 

# We can plot the residuals against the individual Xs again. 

plot(airquality$Solar.R, residuals(logmod))
plot(airquality$Wind, residuals(logmod))
plot(airquality$Temp, residuals(logmod))
# Impression: Most of the curvatures are gone indeed. We don't have to worry
# about adding quadratic terms anymore, phew!
# (But we will have a lecture about polynomial regression models to learn about how to use them when they are really in order.)

# Q-Q plot of the log-transformed model

qqnorm(residuals(logmod))
qqline(residuals(logmod))
# Impression: Almost all deviation from the normal distribution is gone, except for that outlier.


#-------------------------------------------------
# Remove the outlier and fit the log-transformed model again

# When you spot a regression outlier, you should first check for data entry error.
# If the data have been correctly recorded, then since that outlier is too far away 
# from the rest of the data, I recommend that you remove it and model the rest
# of the data. But don't forget to discuss that outlier separately. Outliers
# often tell you an interesting part of the story.

# You can use the following code to identify the outlier

plot(fitted(logmod), rstudent(logmod), type='n')
text(fitted(logmod), rstudent(logmod), names(rstudent(logmod)))
rstudent(logmod)[21] # double check

# Remove the outlier and call the new data airquality.reduced

airquality.reduced <- airquality[-21, ]

# Fit the log-transformed model on the reduced data

logmod.reduced <- lm(log(Ozone) ~ Solar.R + Wind + Temp, data=airquality.reduced, na.action=na.exclude)

# Residual plot of the log-transformed model

plot(fitted(logmod.reduced), rstudent(logmod.reduced))

# Q-Q plot of the log-transformed model

qqnorm(residuals(logmod.reduced))
qqline(residuals(logmod.reduced))

# Impression: Everything looks almost perfect now. So you may not proceed to 
# interpret the summary table, report the hypothesis test results, and/or 
# construct all kinds of confidence intervals.

summary(logmod.reduced)


#-------------------------------------------------
# Extra: Box-cox transformation

# Use the following code to find out the optimal power lambda value

lambda <- powerTransform(mod)$lambda
lambda

# Transform Y with the lambda power and fit the model again

airquality$bc.Ozone <- airquality$Ozone ^ lambda
bc.mod <- lm(bc.Ozone ~ Solar.R + Wind + Temp, data=airquality, na.action=na.exclude)

# Residual plot of the box-cox-transformed model
plot(fitted(bc.mod), rstudent(bc.mod))

# Q-Q plot of the box-cox-transformed model
qqnorm(residuals(bc.mod))
qqline(residuals(bc.mod))

# Shapiro test
ncvTest(bc.mod)
ncvTest(logmod)

# Impression: The Box-cox indeed removed most of the heteroskedasticity. It's 
# interesting that the outlier is not longer showing. Comparing the ncvTest
# results between the Box-Cox transformed model and the log-transformed model,
# the Box-Cox transformed model gives a larger p-value, which means the residual
# variances are more constant than the log-transformed model. 
# Which transformation do you prefer to use?






