setwd("/home/swaroop/Downloads/Assignments/STP530/HW9")

data <- read.table("CH06PR18.txt", header=FALSE)
names(data) <- c("Y", "X1", "X2", "X3", "X4")

# a

data$x1_centered <- data$X1 - mean(data$X1)
data$x1_centered_squared <- data$x1_centered^2

model <- lm(Y ~ x1_centered + x1_centered_squared + X2 + X4, data=data)

plot(data$Y, fitted(model), xlab = "Observed Rental Rates", ylab = "Fitted Rental Rates", main = "Observed vs Fitted Rental Rates")
abline(0, 1)  # Adds a 45-degree line to the plot
# 
# mod <- lm(Y ~ x1_centered + X2 + X4 + x1_centered_squared, data=data)
# anova(mod)


# # Assuming 'data' is your dataframe and 'model' is your fitted model.
# newdata <- data.frame(
#   x1_centered = 8 - mean(data$X1),
#   x1_centered_squared = (8 - mean(data$X1))^2,
#   X2 = 16,
#   X4 = 250000
# su)
# 
# # Calculate the 95% confidence interval for the mean response
# conf_int <- predict(model, newdata=newdata, interval="confidence", level=0.95)
# 
# # Print the confidence interval
# print(conf_int)

b0_centered <- coef(model)[1]
b1_centered <- coef(model)[2]
b11_centered <- coef(model)[3]
b2 <- coef(model)[4]
b4 <- coef(model)[5]

X1_bar <- mean(data$X1)

# Transform coefficients back to the original scale
b0 <- b0_centered - b1_centered * X1_bar + b11_centered * X1_bar^2
b1 <- b1_centered - 2 * b11_centered * X1_bar
b11 <- b11_centered

cat("Original scale coefficients:\n")
cat("b0:", b0, "\nb1:", b1, "\nb11:", b11, "\nb2:", b2, "\nb4:", b4, "\n")

# Create the string representation of the fitted response function
fitted_function <- paste("Y =", 
                         b0, 
                         "+", b1, "*X1", 
                         "+", b11, "*X1^2", 
                         "+", b2, "*X2", 
                         "+", b4, "*X4")

# Print the fitted response function
cat(fitted_function, "\n")


newdata <- data.frame(
  X1 = 8,  # original X1 value
  X2 = 16, # original X2 value
  X4 = 250000 # original X4 value
)

Y_hat_original <- b0 + b1 * newdata$X1 + b11 * newdata$X1^2 + b2 * newdata$X2 + b4 * newdata$X4

cat("Predicted Y^ on the original scale:", Y_hat_original, "\n")




# Assuming you have the mean of X1 in your dataset
X1_centered <- 8 - mean(data$X1)
X1_centered_squared <- X1_centered^2

new_data <- data.frame(x1_centered = X1_centered, x1_centered_squared = X1_centered_squared, X2 = 16, X4 = 250000)

conf_int <- predict(model, newdata = new_data, interval = "confidence", level = 0.95)
conf_int


