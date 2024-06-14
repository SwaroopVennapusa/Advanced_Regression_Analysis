# Read the dataset from the given path
data <- read.table("/home/swaroop/Downloads/Assignments/STP530/HW5/CH06PR15.txt", quote="\"", comment.char="", header=FALSE)

# Rename the columns for ease of reference
colnames(data) <- c("Y", "X1", "X2", "X3")

# Plot histograms for predictor variables
par(mfrow=c(1,3))  # Setting the plotting area to a 1x3 grid

# 6.15 a)
hist(data$X1, main="Histogram for X1 (Age)", xlab="Age", col="lightblue", border="black")
hist(data$X2, main="Histogram for X2 (Severity of Illness)", xlab="Severity Index", col="lightgreen", border="black")
hist(data$X3, main="Histogram for X3 (Anxiety Level)", xlab="Anxiety Index", col="lightcoral", border="black")


# 6.15 b)
pairs(data, main="Scatter Plot Matrix", pch=19, col="blue")

cor_matrix <- cor(data)
print(cor_matrix)


# 6.15 c)
model <- lm(Y ~ X1 + X2 + X3, data = data)
summary(model)


# Additional question

plot(predict(model), residuals(model), main="Residuals vs Fitted", xlab="Fitted values", ylab="Residuals", pch=20)
abline(h = 0, col="red")

qqnorm(residuals(model))
qqline(residuals(model))

hist(residuals(model), main="Histogram of Residuals", xlab="Residuals")
shapiro.test(residuals(model))


