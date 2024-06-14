# Read the dataset from the given path
data <- read.table("/home/swaroop/Downloads/Assignments/STP530/HW5/CH06PR15.txt", quote="\"", comment.char="", header=FALSE)

# Rename the columns for ease of reference
colnames(data) <- c("Y", "X1", "X2", "X3")

# Question 7.5(a)
# Full Model with all predictors
model_full <- lm(Y ~ X1 + X2 + X3, data = data)

# Model with only X2
model_X2 <- lm(Y ~ X2, data = data)

# Model with X1 given X2
model_X1_given_X2 <- lm(Y ~ X2 + X1, data = data)

# Model with X3 given X1 and X2
model_X3_given_X1_X2 <- lm(Y ~ X2 + X1 + X3, data = data)

# Compute SSR for each model
SSR_X2 = sum((fitted(model_X2))^2) 
SSR_X1_given_X2 = sum((fitted(model_X1_given_X2))^2) - SSR_X2
SSR_X3_given_X1_X2 = sum((fitted(model_X3_given_X1_X2))^2) - sum((fitted(model_X1_given_X2))^2)
SSR_full = sum((fitted(model_full))^2) 

# Compute SSE for the full model
SSE_full = sum(residuals(model_full)^2)

# Total sum of squares
SSTO = sum((data$Y - mean(data$Y))^2)

# Compute df for each model
df_X2 = 1
df_X1_given_X2 = 1
df_X3_given_X1_X2 = 1
df_full = 3
df_error = nrow(data) - 4

# Display ANOVA table
anova_table <- data.frame(
  Source_of_Variation = c("Regression", "X2", "X1|X2", "X3|X1,X2", "Error", "Total"),
  SS = c(SSR_full, SSR_X2, SSR_X1_given_X2, SSR_X3_given_X1_X2, SSE_full, SSTO),
  df = c(df_full, df_X2, df_X1_given_X2, df_X3_given_X1_X2, df_error, nrow(data) - 1),
  MS = c(SSR_full/df_full, SSR_X2/df_X2, SSR_X1_given_X2/df_X1_given_X2, SSR_X3_given_X1_X2/df_X3_given_X1_X2, SSE_full/df_error, SSTO/(nrow(data) - 1))
)

print(anova_table)



# Question 7.5(b)

anova_results <- anova(model_X1_given_X2, model_X3_given_X1_X2)
print(anova_results)

p_value <- 1 - pf(3.60, 1, 41)
p_value



# Question 7.6
m.F <- model_X3_given_X1_X2

# Reduced model only with X1
m.R <- lm(Y ~ X1, data = data)

# Perform the F-test comparing the two models
test <- anova(m.R, m.F)
p_value <- test$`Pr(>F)`[2] 
p_value

rsq::rsq.partial(objF = m.F, objR = m.R)


# Question 7.9
# Adjusting for the specified hypotheses
# The full model
data.full <- data
data.full$Y <- data.full$Y
m.F <- lm(Y ~ X1 + X2 + X3, data = data.full)

# The reduced model
data.reduced <- data
data.reduced$Y <- with(data, Y + X1)  # Adjusting the response for the beta1 = -1.0 hypothesis.
m.R <- lm(Y ~ X3, data = data.reduced)  # No need to include X2 as beta2 = 0

# The F-test comparing the two models
test_result <- anova(m.R, m.F)
print(test_result)

# Effect size: partial R-squared
partial_r2 <- rsq::rsq.partial(objF = m.F, objR = m.R)
print(partial_r2)
