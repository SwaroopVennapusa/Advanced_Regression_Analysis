
rm(list=ls()) # Clean up the workspace for the new analysis

# Set the following to your own folder
setwd("~/Dropbox (ASU)/ASU/Course/STP530/class demos/Lecture 5_multiple regression II") 

# Load the library. If you haven't installed it yet, install it first.
library(car)
library(rsq)

# Read in data, check data, attach data
studio <- read.csv("studio_data.csv")
head(studio)
n <- nrow(studio)
attach(studio)


#---------------------------------------------------------------------- 
# F-test of regression relation (The global F-test)

m <- lm(sales ~ n.youth + income)
summary(m)

# Verify with manual calculation
anova(m)
MSR <- (23371.8 + 643.5) / (1 + 1)
MSE <- 121.2
F.obs <- MSR / MSE
F.obs
1 - pf(q = F.obs, df1 = 2, df2 = 18)


#---------------------------------------------------------------------- 
# Extra sum of squares

# The code anova(your.lm.model) gives you the extra sum of squares for adding 
# each term, following the order in which you specified the model.

m.X1 <- lm(sales ~ n.youth)
summary(m.X1)
anova(m.X1)

m.X1X2 <- lm(sales ~ n.youth + income)
summary(m.X1X2)
anova(m.X1X2)

m.X2 <- lm(sales ~ income)
summary(m.X2)
anova(m.X2)

m.X2X1 <- lm(sales ~ income + n.youth)
summary(m.X2X1)
anova(m.X2X1)

# Note: m.X1X2 and m.X2X1 include the two predictors in different orders. 
# The anova() gives different extra sums of squares, but the total SSRs, which
# is the summation of all extra SS's are the same, and the SSEs (residual) are
# the same. The summary(lm()) results are the same for the two models.


#---------------------------------------------------------------------- 
# Partial coefficient of determination

SSE.1 <- sum((sales - predict(m.X1))^2)
SSE.12 <- sum((sales - predict(m.X1X2))^2)

R2.Y2_1 <- (SSE.1 - SSE.12) / SSE.1
R2.Y2_1

# You can also use the other version of the formula:
# R2_{Y2|1} = SSR(X2|X1) / SSE(X1)
anova(m.X1X2)
R2.Y2_1 <- 643.5 / SSE.1
R2.Y2_1 

# Using the rsq.partial function in the rsq package 
rsq::rsq.partial(objF = m.X1X2, objR = m.X1)


#---------------------------------------------------------------------- 
# Partial coefficient of correlation

sqrt(R2.Y2_1)

# The above is equivalent to the following:
m1 <- lm(sales ~ n.youth)
m2 <- lm(income ~ n.youth)
cor(residuals(m1), residuals(m2))


#---------------------------------------------------------------------- 
# The general linear test approach

m_F <- lm(sales ~ n.youth + income)
m_R <- lm(sales ~ n.youth)

anova(m_R, m_F)

# Manually compute to verify
SSE.R <- sum((sales - predict(m_R))^2)
SSE.F <- sum((sales - predict(m_F))^2)
F.obs <- ((SSE.R - SSE.F) / 1) / (SSE.F / (n - 3))
1 - pf(q = F.obs, df1 = 1, df2 = n - 3)


#----------------------------------------------------------------------
# Standardized transformation

sales.std <- scale(sales)
n.youth.std <- scale(n.youth)
income.std <- scale(income)

m.std <- lm(sales.std ~ n.youth.std + income.std - 1)
summary(m.std)

vif(m.std) 
# We get the same VIF, so standardizing doesn't help with multicollinearity 
# among different variables. (It will help with multicollinearity among 
# different-ordered terms of the same variable in polynomial models, and with 
# interaction models.)


#---------------------------------------------------------------------- 
# Correlational transformation

sales.cor.std <- scale(sales) / sqrt(n-1)
n.youth.cor.std <- scale(n.youth) / sqrt(n-1)
income.cor.std <- scale(income) / sqrt(n-1)

m.cor.std <- lm(sales.cor.std ~ n.youth.cor.std + income.cor.std)
summary(m.cor.std)


#---------------------------------------------------------------------- 
# Solving standardized regression without transforming variables

r_xx <- cor(studio[, 2:3])
r_yx <- matrix(c(cor(sales, n.youth), cor(sales, income)), nrow=2)
solve(r_xx) %*% r_yx # The results equal the coefficients you get from m3




