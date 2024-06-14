install.packages("readxl")
install.packages("car")
# install.packages("emmeans")

library(readxl)
library(car)
# library(emmeans)

data <- read_excel("/home/swaroop/Downloads/Assignments/STP530/project/clean_data.xlsx")

# Exploratory Data Analysis
# Assuming that values greater than 200 are impossible for MPG
data <- data[data$`Fuel Economy(MPG)` <= 200, ]

hist(data$`Fuel Economy(MPG)`, main="Distribution of Fuel Economy", xlab="MPG", breaks=20)
summary(data$`Fuel Economy(MPG)`)

plot(data$`Rated Horsepower`, data$`Fuel Economy(MPG)`, main="MPG vs Horsepower", xlab="Rated Horsepower", ylab="MPG")
cor(data$`Rated Horsepower`, data$`Fuel Economy(MPG)`, use="complete.obs")

plot(data$`Equivalent Test Weight (lbs.)`, data$`Fuel Economy(MPG)`, main="MPG vs Vehicle Weight", xlab="Vehicle Weight", ylab="MPG")
cor(data$`Equivalent Test Weight (lbs.)`, data$`Fuel Economy(MPG)`, use="complete.obs")


boxplot(data$`Fuel Economy(MPG)` ~ data$`Vehicle Manufacturer Name`, main="MPG by Manufacturer", xlab="Manufacturer", ylab="MPG", las=2)
# 'las=2' makes the labels perpendicular to the axis for readability


# Question 1 : Do any car manufacturers consistently outperform others in fuel efficiency? 


# Assuming your data frame is named data and 'Manufacturer' and 'MPG' are the correct column names
anova_result <- aov(`Fuel Economy(MPG)` ~ `Vehicle Manufacturer Name`, data = data)
summary(anova_result)

# # Run post-hoc test using 'emmeans' instead of 'TukeyHSD'
# emmeans_result <- emmeans(anova_result, pairwise ~ `Vehicle Manufacturer Name`)
# emmeans_result$contrasts


# Question 2 and 3

# First let's fit a linear regression model with both main effects and interaction term
model <- lm(`Fuel Economy(MPG)` ~ `Rated Horsepower` + `Equivalent Test Weight (lbs.)` + `Rated Horsepower`:`Equivalent Test Weight (lbs.)`, data = data)

# View the summary of the model
summary(model)
vif(model)
vif(model, type = "predictor")


# Selecting relevant columns for the pairs plot
data_selected <- data[, c("Fuel Economy(MPG)", "Rated Horsepower", "Equivalent Test Weight (lbs.)")]

# Creating the pairs plot
pairs(data_selected)


# After inspecting the scatter plots, comparing different models with linear model
model_poly <- lm(`Fuel Economy(MPG)` ~ `Rated Horsepower` + I(`Rated Horsepower`^2) + `Equivalent Test Weight (lbs.)` + I(`Equivalent Test Weight (lbs.)`^2), data = data)
summary(model_poly)

model_poly_interact <- lm(`Fuel Economy(MPG)` ~ `Rated Horsepower`*`Equivalent Test Weight (lbs.)` + I(`Rated Horsepower`^2) + I(`Equivalent Test Weight (lbs.)`^2), data = data)
summary(model_poly_interact)


# After reviewing the summary of the model
model_revised <- lm(`Fuel Economy(MPG)` ~ `Rated Horsepower` + I(`Rated Horsepower`^2) + `Equivalent Test Weight (lbs.)` + `Rated Horsepower`:`Equivalent Test Weight (lbs.)`, data = data)
summary(model_revised)
vif(model_revised)
vif(model_revised, type = "predictor")

# Checking whether we need linear term for Horsepower(This is not necessary because when we consider quadratic term we should consider it's respective linear term by default)
full_model <- lm(`Fuel Economy(MPG)` ~ `Rated Horsepower` + I(`Rated Horsepower`^2), data = data)
summary(full_model)
reduced_model <- lm(`Fuel Economy(MPG)` ~ I(`Rated Horsepower`^2), data = data)
summary(reduced_model)
anova(reduced_model, full_model)


# As we have quadratic and interaction term, let's center the predictors
mean_hp <- mean(data$`Rated Horsepower`, na.rm = TRUE)
mean_weight <- mean(data$`Equivalent Test Weight (lbs.)`, na.rm = TRUE)

data$centered_hp <- data$`Rated Horsepower` - mean_hp
data$centered_weight <- data$`Equivalent Test Weight (lbs.)` - mean_weight

model_revised_centered <- lm(`Fuel Economy(MPG)` ~ centered_hp + I(centered_hp^2) + centered_weight + centered_hp:centered_weight, data = data)
summary(model_revised_centered)


# Hypothesis test to check whether we can drop the square term or not

# Step 1: Assumptions:
# The errors, epsilon , are assumed to be independent and identically distributed (i.i.d. following a normal distribution with mean 0 and constant variance sigma^2.

# Step 2: Hypothesis:
#   Full Model (includes beta_3​ for x1^2​):
#   This model assumes that the squared term of x1​ contributes to explaining the variance in the response variable YY.
# E(Y)=beta_0+beta_1×x1+beta_2×X2+beta_3×x1^2+beta_4×X4
# 
# Reduced Model (excludes beta_3 for x1^2):
#   This model assumes that the squared term of x1x1​ is not needed to explain the variance in YY, and it is excluded from the model.
# E(Y)=beta_0+beta_1×x1+beta_2×X2+beta_4×X4
# 
# Hypotheses:
#   Null Hypothesis (H0): The coefficient of the squared term (beta_3​) is equal to zero, indicating that x1^2 does not have a significant effect on YY.
# H0:beta_3=0
# 
# Alternative Hypothesis (H1H1​): The coefficient of the squared term (beta_3​) is not equal to zero, indicating that x1^2 does have a significant effect on YY.
# H1:beta_3!=0

# Step 3: Test statistic
stat_model <- lm(`Fuel Economy(MPG)` ~ centered_hp + centered_weight + centered_hp:centered_weight + I(centered_hp^2), data = data)

anova_result <- anova(stat_model)
anova_result

# Step 4: P-value

# From the ANOVA table, the p-value for `I(centered_hp^2)` is less than 2.2e-16.

# Step 5: Conclusion

# Assuming the level of significance is alpha = 0.05. Because the p-value for `I(centered_hp^2)` is less than alpha = 0.05 (in fact, it's much smaller, indicating a highly significant result), we reject H_0 at the significance level of 0.05. This means that the full model, which includes the squared term of the centered `Rated Horsepower` variable, fits the data significantly better than the reduced model without this term. Therefore, we conclude that the squared term of `Rated Horsepower`—after centering—does contribute to explaining the variance in `Fuel Economy(MPG)` and should be retained in the model.



# Residual plot for homoskedacity
plot(fitted(model_revised), residuals(model_revised),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# Normality of residuals
qqnorm(residuals(model_revised))
qqline(residuals(model_revised), col = "red")

# Outliers detection
studentized_residuals <- rstudent(model_revised)
plot(studentized_residuals,
     ylab = "Studentized Residuals",
     main = "Studentized Residuals")
abline(h = c(-3, 3), col = "red")

# Influence measures
# Cook's distance
plot(cooks.distance(model_revised), type = "h", main = "Cook's Distance")
# Leverage plot
plot(hatvalues(model_revised), type = "h", main = "Hat Values (Leverage)")
