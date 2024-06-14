
# Clean up the workspace for the new analysis
rm(list=ls()) 

# Set the following to YOUR OWN folder
setwd("~/Dropbox (ASU)/ASU/Course/STP530/class demos/Lecture 3_diagnostics and transformation")

# Import the dataset. The txt data file needs to exist in the folder above.
mydata <- read.table("~/Downloads/Assignments/STP530/HW1/CH01PR28.txt", quote="\"", comment.char="")
head(mydata)

# Rename the columns
colnames(mydata) <- c("crime.rate", "pct.HS") # Rename the columns
head(mydata)

# Fit the linear regression model
m <- lm(crime.rate ~ pct.HS, data=mydata)
summary(m)

# Manually calculate the elements in the ANOVA table
Y <- mydata$crime.rate
Y.bar <- mean(Y)
Y.hat <- predict(m)

SSTO <- sum((Y - Y.bar) ^ 2)
SSR <- sum((Y.hat - Y.bar) ^ 2)
SSE <- sum((Y - Y.hat) ^ 2)

n <- nrow(mydata)
p <- 2

df.TO <- n - 1
df.R <- p - 1
df.E <- n - p

MSTO <- SSTO / df.TO
MSR <- SSR / df.R
MSE <- SSE / df.E

SSTO; SSR; SSE
df.TO; df.R; df.E
MSTO; MSR; MSE

# Get the (part of the) anova table automatically
anova(m)
# This table does not have the row for the "total"s. So you'll need to manually
# add up the SSR and SSE to get SSTO. You can also add up the DFs. Remember to
# not add up the MS terms, instead, MSTO = SSTO / df.TO


# Find the p-value of the F-test manually
pf(q=16.83, df1=(p-1), df2=(n-p), lower.tail=F)
# 1-pf(q=16.83, df1=(p-1), df2=(n-p)) # It is same as above i.e lower.tail=False, by default it is true

# Pearson co-relation r
cor(x = mydata$pct.HS, y = mydata$crime.rate)
sqrt(0.1703) # This value is from summary(m). It gives same as above but misses on the sign, which we can determine using the plot
