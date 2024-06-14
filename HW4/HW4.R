X <- c(1.0, 0.0, 2.0, 0.0, 3.0, 1.0, 0.0, 1.0, 2.0, 0.0)
Y <- c(16.0, 9.0, 17.0, 12.0, 22.0, 13.0, 8.0, 15.0, 19.0, 11.0)
model <- lm(Y ~ X)
summary(model)
Y_hat <- predict(model)
Y_bar <- mean(Y)
SSR <- sum((Y_hat - Y_bar)^2)
SSE <- sum((Y - Y_hat)^2)
SSTO <- sum((Y - Y_bar)^2)
p <- 2  # number of parameters (intercept + slope)
n <- length(Y)

MSR <- SSR / (p - 1)
MSE <- SSE / (n - p)
MSTO <- SSTO / (n - 1)

# Question 2.25(a)
anova(model)

# Question 2.25(b)
p_value <- 1 - pf(72.73, 1, 8)

# Question 2.25(d)
r <- cor(X, Y)


# Question 3.25
# Load the necessary libraries
library(faraway)
library(car)
library(Hmisc)
library(psych)

# Loading the data
data <- read.table("https://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Appendix%20C%20Data%20Sets/APPENC02.txt", header=FALSE)

# Extracting relevant columns
Y <- data$V8
X1 <- data$V5  # total population
X2 <- data$V9  # number of hospital beds
X3 <- data$V16 # total personal income

# Fitting the model for total population
model1 <- lm(Y ~ X1)
summary(model1)
# Residuals vs. total population
par(mfrow=c(1,2))
plot(X1, residuals(model1), main="Residuals vs Total Population")
abline(h=0, col="red")

# QQ-plot for residuals
qqnorm(residuals(model1), main="QQ-Plot of residuals for Total Population")
qqline(residuals(model1))

# Fitting the model for number of hospital beds
model2 <- lm(Y ~ X2)
summary(model2)
# Residuals vs. number of hospital beds
plot(X2, residuals(model2), main="Residuals vs Number of Hospital Beds")
abline(h=0, col="red")

# QQ-plot for residuals
qqnorm(residuals(model2), main="QQ-Plot of residuals for Number of Hospital Beds")
qqline(residuals(model2))

# Fitting the model for total personal income
model3 <- lm(Y ~ X3)
summary(model3)
# Residuals vs. total personal income
plot(X3, residuals(model3), main="Residuals vs Total Personal Income")
abline(h=0, col="red")

# QQ-plot for residuals
qqnorm(residuals(model3), main="QQ-Plot of residuals for Total Personal Income")
qqline(residuals(model3))


# # Residual plots
# par(mfrow = c(3, 2))  # Arrange the plots in a grid of 3 rows and 2 columns
# 
# plot(cdi_data$Population, residuals(model_population), main="Residuals vs Population")
# abline(h = 0, col = "red")
# 
# plot(cdi_data$Beds, residuals(model_beds), main="Residuals vs Beds")
# abline(h = 0, col = "red")
# 
# plot(cdi_data$Income, residuals(model_income), main="Residuals vs Income")
# abline(h = 0, col = "red")
# 
# 
# # QQ plots
# qqnorm(residuals(model_population))
# qqline(residuals(model_population))
# title("QQ-plot for Population model")
# 
# qqnorm(residuals(model_beds))
# qqline(residuals(model_beds))
# title("QQ-plot for Beds model")
# 
# qqnorm(residuals(model_income))
# qqline(residuals(model_income))
# title("QQ-plot for Income model")

