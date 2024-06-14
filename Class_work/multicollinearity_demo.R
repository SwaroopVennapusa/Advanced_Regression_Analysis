
# STP 530 demo of multicollinearity

#-------------------------------------------------------------
# PROBLEM 1
#-------------------------------------------------------------

# This project aims to identify factors that can explain variations in salaries 
# among programmers and engineers in the Silicon Valley. We will use data collected 
# during the 2000 U.S. Census. The data contain information of 20090 programmers 
# and engineers in the Silicon Valley area, and are available in the R package freqparcoord.

library(freqparcoord)
data(prgeng)
help(prgeng)
str(prgeng)

# Fit a regression model with a select set of predictors. 
# (This model is presented for demoing multicollinearity only. Do not follow it 
# for your final project. The data and the problem are much more sophisticated.)
# Note you need to first convert cit and sex into the factor type

prgeng$cit <- factor(prgeng$cit, levels=1:5, labels=c("Born.US", "Born.islands", "Born.abroad", "Naturalized", "Non.Citizen"))
prgeng$cit <- relevel(prgeng$cit, ref="Non.Citizen")

prgeng$sex <- factor(prgeng$sex, levels=1:2, labels=c("male", "female"))

m1 <- lm(wageinc ~ age + cit + educ + sex + wkswrkd + yrentry, data=prgeng)

vif(m1)
# yrentry has very high VIF. cit has the second highest VIF.
# Take a look at the coding system of yrenry and cit.

# yrentry: Year of entry to the U.S. (0 for natives)

# cit: Citizenship status:
# 1: Yes, born in the United States
# 2: Yes, born in the U.S. islands
# 3: Yes, born abroad of American parent or parents
# 4: Yes, U.S. citizen by naturalization
# 5: No, not a citizen of the United States

# Notice that all cit = 1 individuals also have yrentry = 0
# That is confirmed by the following plots

with(prgeng, plot(cit, yrentry))
with(prgeng, sunflowerplot(cit, yrentry)) # A sunflower plot shows the volume of cases sharing the same coordinates.

# Removing yrentry from the model resolved multicollinearity.

m2 <- lm(wageinc ~ age + cit + educ + sex + wkswrkd, data=prgeng)
vif(m2)

# Also notice that the coefficients for cit changed dramatically while the coefficients 
# for other predictors and model predictions remain stable.

summary(m1)
summary(m2)


#-------------------------------------------------------------
# PROBLEM 2
#-------------------------------------------------------------

# The research problem is to model the price charged for trucking service in Florida. 
# In the early 1980s, several states removed regulatory constraints on the rate charged for 
# intrastate trucking services, Florida being the first one to embark on a deregulation policy. 
# The objective of the regression analysis is twofold: 
# (1) assess the impact of deregulation on the prices charged for trucking service in Florida, and 
# (2) estimate a model of supply price for predicting future prices.

# DATA. The data (n = 30) were obtained from a particular carrier whose trucks originated from 
# either the city of Jacksonville or Miami. The dependent variable of interest is the price in 
# dollars charged per ton-mile. The following table lists the variables in the dataset, including 
#  the response variable, potential predictors, and the natural log transformation of the response 
# variable.

#-------------------------------------------------------------
# Prepare environment

rm(list=ls())
setwd("~/Dropbox (ASU)/ASU/Course/STP530/class demos/Lecture 6_multicollinearity") 
library(car)

#-------------------------------------------------------------
# Prepare and check data

# Load data
TRUCKING <- read.table("TRUCKING.txt", header=T, stringsAsFactors=T)

# Check data
head(TRUCKING)
str(TRUCKING) # Have all categorical predictors been read in as factors?
dim(TRUCKING) 
attach(TRUCKING)

# Check marginal relationship
pairs(TRUCKING)

#-------------------------------------------------------------
# Fit the regression models with all available predictors

m1 <- lm(PRICPTM ~ DISTANCE + WEIGHT + PCTLOAD + ORIGIN + MARKET + DEREG + CARRIER + PRODUCT, data=TRUCKING)

# Error message: "...contrasts can be applied only to factors with 2 or more levels"

table(TRUCKING$CARRIER) 
# All cases in the dataset share the same value, which causes the singularity problem 
# when taking the inverse of X'X for solving for b=(X'X)^{-1}X'Y. See demo below.

# First set up the X matrix

X <- data.frame(1, TRUCKING[, 2:9])
X
X[, 5] <- as.numeric(X[, 5]) - 1
X[, 6] <- as.numeric(X[, 6]) - 1
X[, 7] <- as.numeric(X[, 7]) - 1
X[, 8] <- as.numeric(X[, 8]) - 1
X <- as.matrix(X)
X

# Then attempt to take the inverse of X'X. Returns an error.

solve(t(X) %*% X)

# You cannot include any single-value predictor in a regression model.
# So we exclude CARRIER.

m2 <- lm(PRICPTM ~ DISTANCE + WEIGHT + PCTLOAD + ORIGIN + MARKET + DEREG + PRODUCT, data=TRUCKING)

# Check the VIF values of m2 predictors

vif(m2)
# WEIGHT and PCTLOAD have very high VIF values. The scatter plot shows they are almost 
# perfectly correlated. That likely resulted from having a single carrier in this dataset.

# Remove one of the above two predictors, get rid of multicollinearity

m3 <- lm(PRICPTM ~ DISTANCE + WEIGHT + ORIGIN + MARKET + DEREG + PRODUCT, data=TRUCKING)
vif(m3)

#-------------------------------------------------------------
# PROBLEM 3
#-------------------------------------------------------------

# The purpose of the study is to find out the relationship between the sale price 
# of a property and the following variables:
#   1. Appraised land value of the property
#   2. Appraised value of the improvements on the property 
#   3. Neighborhood in which the property is listed
#   
# The objectives of the study include:
#   1. To determine whether the data indicate that appraised values of land and improvements are related to sale prices.
#   2. To determine whether the relationship between the appraised values of land and improvements and sale prices is the 
#      same for different neighborhoods. In other words, do the appraisers use the same appraisal criteria for different neighborhoods?
#   3. To acquire the prediction equation relating appriased value of land and improvements to sale price.
# 
# The data “TAMSALES4.txt” (n = 176) were supplied by the property appraiser’s office of Hills- borough County, Florida, 
# and consist of the appraised land and improvement values and sale prices for residential properties sold in the city of 
# Tampa, Florida, during the period May 2008 to June 2009. Four neighborhoods (Hyde Park, Cheval, Hunter’s Green, and 
# Davis Isles), each relatively homogeneous but dif- fering sociologically and in property types and values, were included. 

#------------------------------------------------------------------------------- 
# Read in data

house <- read.table("TAMSALES4.txt", header=T, stringsAsFactors=T)
head(house)
str(house)

#------------------------------------------------------------------------------- 
# Pairwise correlations are only moderate

with(house, cor(data.frame(LAND, IMP, TOTVAL)))
with(house, pairs(data.frame(LAND, IMP, TOTVAL)))

# Extremely high VIFs when all three predicts are included in a model

m1 <- lm(SALES ~ LAND + IMP + TOTVAL, data=house)
vif(m1)

# Seeing LAND + IMP and TOTVAL side-by-side confirms that TOTVAL = LAND + IMP

with(house, data.frame(LAND_IMP=LAND + IMP, TOTVAL))

# Remove TOTVAL from the model resolves the multicollinearity problem

m2 <- lm(SALES ~ LAND + IMP, data=house)
vif(m2)







