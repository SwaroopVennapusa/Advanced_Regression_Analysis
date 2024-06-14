
# Clean up the workspace for the new analysis
rm(list=ls()) 

# Set the following to YOUR OWN folder
#setwd("~/Dropbox (ASU)/ASU/Course/STP530/class demos/Lecture 2_inferences")

# Import the dataset. The txt data file needs to exist in the folder above.
#mydata <- read.table("CH01PR28.txt")
mydata <- read.table("~/Downloads/Assignments/STP530/HW1/CH01PR28.txt", quote="\"", comment.char="")
head(mydata)

# Rename the columns
colnames(mydata) <- c("crime.rate", "pct.HS") # Rename the columns
head(mydata)

# Fit the linear regression model
m <- lm(crime.rate ~ pct.HS, data=mydata)
summary(m)

# 95% confidence intervals for the intercept and slope parameters
confint(m, level = .95)

# manual calculation of the 95% CI of the slope parameter
-170.58 - qt(p=.975, df=82) * 41.57 # lower limit
-170.58 + qt(p=.975, df=82) * 41.57 # upper limit

# 95% confidence interval and prediction interval for X=60
new.data <- data.frame("pct.HS"=60)
predict(m, newdata=new.data)
predict(m, interval="confidence", newdata=new.data, level=.95) # confidence interval
predict(m, interval="prediction", newdata=new.data, level=.95) # prediction interval

# The block below calculates the 95% PREDICTION interval for X=60 manually
y.hat <- coef(m)[1] + coef(m)[2] * 60
names(y.hat) <- NULL
n <- nrow(mydata)
t.crit <- qt(p=.975, df=(n-2))
s <- sqrt(sum((mydata$crime.rate - predict(m)) ^ 2) / (n - 2))
x.bar <- mean(mydata$pct.HS)
se <- s * sqrt(1 + 1 / n + (60 - x.bar) ^ 2 / sum((mydata$pct.HS - x.bar) ^ 2))
y.hat - t.crit * se
y.hat + t.crit * se

# Plot 95% confidence band and prediction band
new.data <- data.frame("pct.HS"=(60:90))
my.pred <- predict(m, interval="confidence", newdata=new.data, level=.95)
plot(crime.rate ~ pct.HS, data=mydata, pch=16, col="darkgrey",
     main="95% confidence band and prediction band",
     xlab="% of individuals having at least a high school diploma",
     ylab="Crime reported per 100,000 residents")
lines(60:90, my.pred[, 1], lwd=2)
lines(60:90, my.pred[, 2], col="blue", lty=2, lwd=2)  
lines(60:90, my.pred[, 3], col="blue", lty=2, lwd=2)
my.pred <- predict(m, interval="predict", newdata=new.data, level=.95)
lines(60:90, my.pred[, 2], col="forestgreen", lty=2, lwd=2)  
lines(60:90, my.pred[, 3], col="forestgreen", lty=2, lwd=2)
legend("topright", legend=c("confidence", "prediction"), 
       lty=2, lwd=2, col=c("blue", "forestgreen"), cex=1)

