
# Import the dataset
mydata <- read.table("~/Downloads/Assignments/STP530/HW1/CH01PR28.txt", quote="\"", comment.char="")

# Rename the columns
colnames(mydata) <- c("crime.rate", "education")


# Question a 
# Run the regression model
m <- lm(crime.rate ~ education, data = mydata)
summary(m)

# Plot the points and the line
plot(crime.rate ~ education, data = mydata) # first option is formula which is y~x. second option is data
abline(coef(m))


# Question b.1

# The difference in the mean crime rate for two counties whose high-school graduation rates differ by one percentage point. This is the coefficient of the variable "education" which is -170.58. This means for every one-percentage-point increase in the percentage of individuals having at least a high-school diploma, the crime rate decreases by 170.58 crimes per 100,000 residents, on average.
coef(m)["education"]


# Question b.2

# The mean crime rate last year in counties with high school graduation percentage X = 80.
# Using the regression equation:
# Y^=20517.60−170.58×80=20517.60−13646.4=6871.2
# So, the predicted crime rate in counties with a graduation percentage of 80% is 6,871.2 crimes per 100,000 residents.
predict(m, newdata = data.frame(education = 80))

# Question b.3

#This refers to the residual for the 10th observation. From the output, epsilon(10) is 1401.566. This means that the actual crime rate for the 10th county is 1,401.566 crimes per 100,000 residents higher than what the regression line predicts.
# Predict
y.hat <- predict(m)
e <- mydata$crime.rate - y.hat
e[10]

# Get residuals directly
# residuals(m)[10]
# m$residuals[10]


# Question b.4

# n is the number of observations (84 counties in this case).
# p is the number of parameters in the model. Since we have an intercept and one predictor (education), p=2
# ei is the residual for the ith observation (difference between observed and predicted values).
# sigma^2 = sum(ei ^ 2) / (n-p)
# Residual variance
sqrt(sum(e ^ 2) / (84 - 2)) # residual standard error
sum(e ^ 2) / (84 - 2) # the square of the above
