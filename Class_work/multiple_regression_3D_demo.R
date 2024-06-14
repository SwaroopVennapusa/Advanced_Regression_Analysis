
rm(list=ls()) # Clean up the workspace for the new analysis

# Set the following to your own folder
setwd("~/Dropbox (ASU)/ASU/Course/STP530/class demos/Lecture 4_multiple regression I") 


#------------------------------------------------------------
# Install and load the add-on packages
#install.packages("rgl")
#install.packages("psych")
library(rgl)
library(psych)


#------------------------------------------------------------
# Read in data (textbook Section 6.9 p.236)
studio <- read.csv("studio_data.csv")
head(studio)
str(studio)
attach(studio)


#------------------------------------------------------------
# Inspect and visualize the data
psych::describe(studio)
hist(sales)
hist(n.youth)
hist(income)
pairs(studio)
cor(studio)


#------------------------------------------------------------
# Fit multiple regression model
m <- lm(sales ~ n.youth + income)
summary(m)


#------------------------------------------------------------------------------- 
# 3D plot for the regression surface

par3d(cex=2)
plot3d(n.youth, income, sales, col="red", size=20)

x <- seq(from=min(n.youth), to=max(n.youth), length.out=10)
y <- seq(from=min(income), to=max(income), length.out=10)
z <- matrix(NA, length(x), length(y))
for (i in 1:length(x)){
  for (j in 1:length(y)){
    z[i, j] <- sum(coef(m) * c(1, x[i], y[j]))
  }
}
surface3d(x, y, z, color="yellow")


#------------------------------------------------------------------------------- 
# Visualize the regression surface of an interaction regression model

m.int <- lm(sales ~ n.youth + income + n.youth * income)
summary(m.int)


par3d(cex=2)
plot3d(n.youth, income, sales, col="red", size=20)

x <- seq(from=min(n.youth), to=max(n.youth), length.out=10)
y <- seq(from=min(income), to=max(income), length.out=10)
z <- matrix(NA, length(x), length(y))
for (i in 1:length(x)){
  for (j in 1:length(y)){
    z[i, j] <- sum(coef(m.int) * c(1, x[i], y[j], x[i]*y[j]))
  }
}
surface3d(x, y, z, color="yellow")


#------------------------------------------------------------
# Residual diagnostics

# (1) Check whether the relationship between Y and each X is linear. 
# Plot the residuals against each X.
plot(n.youth, residuals(m))
plot(income, residuals(m))
# Impression: The residuals do not seem to relate to either X1 or X2 in a 
# systematic manner. So the first-order terms of the two predictors in model m
# seems sufficient.

# (2) Check for outliers.
# Plot the studentized residuals against Y-hat.
plot(predict(m), rstudent(m))
# Impression: All studentized residuals are in a reasonable range given the 
# approximate 1-2-3 rule. No excessive outliers are seen.

# (3) Check for heteroskedasticity
# Plot the residuals against Y-hat
plot(predict(m), residuals(m))
# Impression: The vertical spread of the points are roughly constant across
# different X values. No concern of keteroskedasticity.

# (4) Check whether the residuals are normally distributed.
# Create the QQ-plot of the residuals.
qqnorm(residuals(m))
qqline(residuals(m))
# The points are not well-aligned with the reference line. So the distribution
# is not exactly normal. But keep in mind the sample size is very small (n = 21).
# Also, there is no excessive skewness. The histogram of the residuals can confirm
# this distribution is reasonably bell-curved.
hist(residuals(m))

# (5) Reflect whether the observations are independent from each other
# You would go to the problem description and the data collection method to find
# hints. Each row in the data represents a city. Is it reasonable to assume the
# cities are independent with respect to the three measures involved in this 
# regression problem?

# Summary:
# This is a case where the data pass all basic diagnostic items easily.
# Now Step 1 of all the hypothesis tests in regression is formally completed.
# It is safe and valid to interpret the hypothesis test results.


#------------------------------------------------------------
# Matrix approaches

# Create matrix from data
X <- as.matrix(cbind(1, studio[ , 2:3]))
Y <- as.matrix(studio[ , 1])


# Estimated coefficients
b <- solve(t(X) %*% X) %*% t(X) %*% Y
b
coef(m) # Compare with results from lm()


# Residual vector
e <- Y - X %*% b
e
print(cbind(Y, X %*% b, e),digits=2)
cbind(e, m$residuals) # Compare


# SSTO and SSR
n <- nrow(studio)
J <- matrix(1, nrow=n, ncol=n)
SSTO <- t(Y) %*% Y - t(Y) %*% J %*% Y / n
SSTO
SSR <- t(b) %*% t(X) %*% Y - t(Y) %*% J %*% Y / n
SSR


# SSE and estimated error variance
SSE <- t(e) %*% e
SSE
SSE <- as.numeric(SSE) # Convert SSE from a matrix object to a scalar object
s2 <- SSE / (nrow(X) - ncol(X))
s2
summary(m)$sigma ^ 2 # Compare


# Estimated variance-covariance matrix of estimated model coefficients
cov.b <- s2 * solve(t(X) %*% X)
cov.b
sqrt(diag(cov.b))
summary(m)$coefficients # Compare


# Y-hat for n.youth=65.4, income=17.6
X_h <- matrix(c(1, 65.4, 17.6), nrow=3)
X_h
Y_h <- t(X_h) %*% b
Y_h
predict(m, newdata=data.frame(n.youth=65.4, income=17.6)) # Compare


# 95% confidence interval for E{Y_h}
s2.Y_h <- s2 * t(X_h) %*% solve(t(X) %*% X) %*% X_h
s2.Y_h
se.Y_h <- sqrt(s2.Y_h)
se.Y_h
t.crit <- qt(p = .975, df = nrow(X) - ncol(X))
t.crit

LB <- Y_h - t.crit * se.Y_h
UB <- Y_h + t.crit * se.Y_h
c(LB, UB)

# Interpretation (p.246): with confidence coefficient .95, 
# we estimate that mean sales in cities with target population 
# of 65.4 thousand persons aged 16 years or younger and per 
# capita disposable income of 17.6 thousand dollars are somewhere 
# between 185.3 and 196.9 thousand dollars. 

# Compare with R function
predict(m, newdata=data.frame(n.youth=65.4, income=17.6), 
        interval="confidence", level=.95) 


# 95% prediction interval for Y_h
s2.Y_h.pred <- s2 * (1 + t(X_h) %*% solve(t(X) %*% X) %*% X_h)
s2.Y_h.pred
se.Y_h.pred <- sqrt(s2.Y_h.pred)
se.Y_h.pred
t.crit <- qt(p = .975, df = nrow(X) - ncol(X))
t.crit

LB.pred <- Y_h - t.crit * se.Y_h.pred
UB.pred <- Y_h + t.crit * se.Y_h.pred
c(LB.pred, UB.pred)

# Interpretation: With a confidence coefficient .95, We predict that 
# the sales in a city with a target population of 65.4 thousand persons
# aged 16 years or younger and a per capita disposable income of 17.6
# thousand dollars are somewhere between 167.3 and 214.9 thousand dollars. 

# Compare with R function
predict(m, newdata=data.frame(n.youth=65.4, income=17.6), interval="prediction", level=.95)


# The Hat matrix
H <- X %*% solve(t(X) %*% X) %*% t(X)
H


# Variance-covariance matrix of residuals
I <- diag(nrow(X))
cov.e <- s2 * (I - H)
cov.e


# Studentized residual
r <- e / sqrt(diag(cov.e))
r

# Note: our textbook and common R packages have different definitions
# of "studentized residual". The "studentized residual" described 
# in our textbook, which is given by r above, is called "standardized
# residual" by the R packages and is given by the rstandard() function.
# What the R packages call "studentized residuals" and produce with the
# rstudent() function is called "studentized deleted residuals" (p.396, equations 10.24a, 10.26). See the comparison below:

# The "studentized deleted residuals" (textbook Equation 10.26, p.396)
t <- e * sqrt((nrow(X) - ncol(X) - 1)/ (SSE * diag(I - H) - e ^ 2)) 

cbind(r, rstandard(m), t, rstudent(m))


