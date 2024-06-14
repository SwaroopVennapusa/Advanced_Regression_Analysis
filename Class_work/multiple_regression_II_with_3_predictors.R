
rm(list=ls()) # Clean up the workspace for the new analysis


# -------------------------------------------------------------------------
# Set the following to your own folder 

setwd("~/Dropbox (ASU)/ASU/Course/STP530/class demos/Lecture 5_multiple regression II") 

# -------------------------------------------------------------------------
# Read in data, check data 

physician.data <- read.table("APPENC02.txt")

head(physician.data)
str(physician.data)


# -------------------------------------------------------------------------
# Create a smaller dataset for analysis 

my.data <- with(physician.data, data.frame(Y = V8, X1 = V5, X2 = V9, X3 = V16))
head(my.data)
# Y: The number of active physicians in the county
# X1: The total population of the county
# X2: The number of hospital beds in the county
# X3: The total personal income of the county (in millions of dollars) 


# -------------------------------------------------------------------------
# Univariate distributions and bivariate relations 

hist(my.data$Y)
hist(my.data$X1)
hist(my.data$X2)
hist(my.data$X3)

cor(my.data)


# -------------------------------------------------------------------------
# Fit the initial model

m1 <- lm(Y ~ X1 + X2 + X3, data = my.data)

# -------------------------------------------------------------------------
# Diagnostics

plot(predict(m1), residuals(m1))
plot(my.data$X1, residuals(m1))
plot(my.data$X2, residuals(m1))
plot(my.data$X3, residuals(m1))

qqnorm(residuals(m1))
qqline(residuals(m1))
hist(residuals(m1))

# Heteroskedasticity and non-normality observed.
# Try transforming Y with log
m2 <- lm(log(Y) ~ X1 + X2 + X3, data = my.data)

plot(predict(m2), residuals(m2))
plot(my.data$X1, residuals(m2))
plot(my.data$X2, residuals(m2))
plot(my.data$X3, residuals(m2))

# Transforming Y only does not solve the problem. Non-linearity is observed in all X's.
# Remember that all Xs are also right-skewed like Y. Try transforming all Xs with log too.
log.data <- with(my.data, data.frame(Y.log = log(Y),
                                     X1.log = log(X1),
                                     X2.log = log(X2),
                                     X3.log = log(X3)))
head(log.data)
m3 <- lm(Y.log ~ X1.log + X2.log + X3.log, data = log.data)

plot(predict(m3), residuals(m3))
plot(predict(m3), rstudent(m3))
qqnorm(residuals(m3))
qqline(residuals(m3))
hist(residuals(m3))
# All look good. 


# -------------------------------------------------------------------------
# Standardize all variables

# Since we are losing the original measures, why not standardize all variables, too.
# This way you can compare the magnitudes of the effects among predictors.

std.log.data <- with(log.data, data.frame(Y.std.log = scale(Y.log),
                                     X1.std.log = scale(X1.log),
                                     X2.std.log = scale(X2.log),
                                     X3.std.log = scale(X3.log)))
head(std.log.data)
m4 <- lm(Y.std.log ~ X1.std.log + X2.std.log + X3.std.log, data = std.log.data)
summary(m4)

# Observe that the intercept is a super tiny number. That is pure rounding errors
# in the computer's computation. A regression model with all variables standardized
# should have an intercept of 0. The following model removes the intercept.

m5 <- lm(Y.std.log ~ -1 + X1.std.log + X2.std.log + X3.std.log, data = std.log.data)
summary(m5)

plot(predict(m5), residuals(m5))
plot(predict(m5), rstudent(m5))
qqnorm(residuals(m5))
qqline(residuals(m5))
hist(residuals(m5))



# -------------------------------------------------------------------------
# The F-test of global model utility

summary(m5)
# You can find quantities needed for the F-test of global model utility on the
# last line of the summary() output. 

# Manually calculate the test-statistic and p-value from the ANOVA table
anova(m5)
SSR <- 354.09 + 32.02 + 13.75
MSR <- SSR / (1 + 1 + 1)
F.obs <- MSR / 0.09
F.obs
pf(q = F.obs, df1 = 2, df2 = 18, lower.tail = F)

# Effect size: The Multiple R-squared is the effect size that goes with the F-test
# of global model utility. Find it in the summary() output.
summary(m5)$r.squared


# -------------------------------------------------------------------------
# The F-test of extra sum of squares 

# To get the extra sums of squares for (1) X2, (2) X1 given X2, and (3) X3 given X1 and X2,
# you need to specify the model following the above order:

m6 <- lm(Y.std.log ~ X2.std.log + X1.std.log + X3.std.log, data = std.log.data)
anova(m6)
# You can test whether X3 can be dropped from the model given that X1 and X2 are 
# retained using quantities given on the row of X3.log.

# Effect size: The partial R-squared is the effect size that goes with the F-test of
# extra sum of squares (and the linear testing approach below).

SSE.m6 <- 39.14

m6_0 <- lm(Y.std.log ~ X2.std.log + X1.std.log, data = std.log.data)
anova(m6_0)
SSE.m6_0 <- 52.89

R2.Y3_12 <- (SSE.m6_0 - SSE.m6) / SSE.m6_0
R2.Y3_12

# Verify with what an add-on function gives:
rsq::rsq.partial(objF = m6, objR = m6_0)


# -------------------------------------------------------------------------
# The general linear testing approach
# Test whether both X2 and X3 can be dropped from the model given that X1 is retained.

# The full model
m.F <- lm(Y.std.log ~ X1.std.log + X2.std.log + X3.std.log, data = std.log.data)
# The reduced model
m.R <- lm(Y.std.log ~ X1.std.log, data = std.log.data)
# The F-test comparing the two models
anova(m.R, m.F)

# Manually compute to verify
anova(m.F)
anova(m.R)
F.obs <- ((84.91 - 39.14) / (438 - 436)) / (39.14 / 436)
F.obs
pf(q = F.obs, df1 = 2, df2 = 436, lower.tail=F)

# Effect size: partial R-squared
rsq::rsq.partial(objF = m.F, objR = m.R)


# -------------------------------------------------------------------------
# The general linear testing approach
# Test whether beta1 = beta2 = beta3 = 0. 
# See how it's equivalent to the F-test of global model utility

# The full model
m.F <- lm(Y.std.log ~ X1.std.log + X2.std.log + X3.std.log, data = std.log.data)
# The reduced model
m.R <- lm(Y.std.log ~ 1, data = std.log.data)
# The F-test comparing the two models
anova(m.R, m.F)
# Effect size: partial R-squared
rsq::rsq.partial(objF = m.F, objR = m.R)

# Compare with the F-test of global utility and the multiple R-squared
summary(m.F)


# -------------------------------------------------------------------------
# The general linear testing approach
# Test whether the effect of X2.std.log on Y.std.log is the same as X3.std.log on Y.std.log

# The full model
m.F <- lm(Y.std.log ~ X1.std.log + X2.std.log + X3.std.log, data = std.log.data)
# The reduced model
X.R <- with(std.log.data, X2.std.log + X3.std.log)
m.R <- lm(Y.std.log ~ X1.std.log + X.R, data = std.log.data)
# The F-test comparing the two models
anova(m.R, m.F)
# Effect size: partial R-squared
rsq::rsq.partial(objF = m.F, objR = m.R)


# -------------------------------------------------------------------------
# The general linear testing approach
# Test whether the beta2 = 5

# The full model
m.F <- lm(Y.std.log ~ X1.std.log + X2.std.log + X3.std.log, data = std.log.data)
# The reduced model
Y.R <- with(std.log.data, Y.std.log - 5 * X2.std.log)
m.R <- lm(Y.R ~ X1.std.log + X3.std.log, data = std.log.data)
# The F-test comparing the two models
anova(m.R, m.F)

# An error occurred above because R wants the response variable to have the same name
# in both models. The following is the fix.

# The full model
data.full <- std.log.data
data.full$Y <- data.full$Y.std.log
m.F <- lm(Y ~ X1.std.log + X2.std.log + X3.std.log, data = data.full)
# The reduced model
data.reduced <- std.log.data
data.reduced$Y <- with(std.log.data, Y.std.log - 5 * X2.std.log)
m.R <- lm(Y ~ X1.std.log + X3.std.log, data = data.reduced)
# The F-test comparing the two models
anova(m.R, m.F)
# Effect size: partial R-squared
rsq::rsq.partial(objF = m.F, objR = m.R)



