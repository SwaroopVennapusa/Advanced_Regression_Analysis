
# STP 530 Adjusted R-square demo

# PROBLEM. The research problem is to model the price charged for trucking service in Florida. 
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
setwd("~/Dropbox (ASU)/ASU/Course/STP530/class demos/Lecture 5_multiple regression II") 

#-------------------------------------------------------------
# Prepare and check data

# Load data
TRUCKING <- read.csv("TRUCKING_R2_demo.csv", stringsAsFactors=T)

# Check data
head(TRUCKING)
str(TRUCKING) # Have all categorical predictors been read in as factors?
dim(TRUCKING) 
attach(TRUCKING)

# Check marginal relationship
pairs(TRUCKING)

#-------------------------------------------------------------
# Fit the regression models. Add one predictor at a time.

# For illustrating the adjusted R-square in this lecture, we will do the following
# to identify the marginal linear association between Y and each X. Later you 
# will learn other functions to do it automatically.

TRUCKING.for.cor <- TRUCKING
TRUCKING.for.cor$ORIGIN <- as.numeric(TRUCKING$ORIGIN)
TRUCKING.for.cor$MARKET <- as.numeric(TRUCKING$MARKET)
TRUCKING.for.cor$DEREG <- as.numeric(TRUCKING$DEREG)

cor(TRUCKING.for.cor)

# From the first row of the above correlation matrix, we learn the order of the marginal 
# linear association with Y from strongest to weakest: 
# DISTANCE, DEREG, WEIGHT, ORIGIN, MARKET
# That's the order we will enter each predictor to the regression model.

# m1

m1 <- lm(LNPRICEPTM ~ DISTANCE)
summary(m1)

ls(summary(m1)) # Take a look at what objects can be extracted from the summary
summary(m1)$r.squared
summary(m1)$adj.r.squared

# We will record the R2 and Adj.R2 for each model and plot them afterwards
R2 <- summary(m1)$r.squared
Adj.R2 <- summary(m1)$adj.r.squared

# m2

m2 <- lm(LNPRICEPTM ~ DISTANCE + DEREG)
summary(m2)
R2 <- c(R2, summary(m2)$r.squared)
Adj.R2 <- c(Adj.R2, summary(m2)$adj.r.squared)

# m3

m3 <- lm(LNPRICEPTM ~ DISTANCE + DEREG + WEIGHT)
summary(m3)
R2 <- c(R2, summary(m3)$r.squared)
Adj.R2 <- c(Adj.R2, summary(m3)$adj.r.squared)

# m4

m4 <- lm(LNPRICEPTM ~ DISTANCE + DEREG + WEIGHT + ORIGIN)
summary(m4)
R2 <- c(R2, summary(m4)$r.squared)
Adj.R2 <- c(Adj.R2, summary(m4)$adj.r.squared)

# m5

m5 <- lm(LNPRICEPTM ~ DISTANCE + DEREG + WEIGHT + ORIGIN + MARKET)
summary(m5)
R2 <- c(R2, summary(m5)$r.squared)
Adj.R2 <- c(Adj.R2, summary(m5)$adj.r.squared)


#-------------------------------------------------------------
# Visualize the trends of R2 and Adjusted R2

# Change the plot margins

par(mar=c(5, 5.5, 4, 2.5))

# Plot R2 and set some graphing parameters

plot(x=1:5,  y=R2, type="b", col="blue", ylim=c(0.4, 1),
     pch=16, cex=1.5, 
     ylab="(Adjusted) R-squared", xlab="Models", cex.lab=1.5,
     xaxt="n", yaxt="n")

# Add points to the plot for Adjusted R2

points(x=1:5,  y=Adj.R2, type="b", col="orange", pch=16, cex=1.5)

# Add axis labels and the legend

axis(1, at=1:5, labels=c("+DISTANCE", "+DEREG", "+WEIGHT", "+ORIGIN", "+MARKET"), cex.axis=1.3)

axis(2, at=seq(0.4, 1, by=0.1), cex.axis=1.3, las=2)

legend("topleft", legend=c("R2", "Adjusted R2"), col=c("blue", "orange"), pch=16)




