mydata <- read.table("~/Downloads/Assignments/STP530/HW2/CH01PR20.txt", quote="\"", comment.char="")
head(mydata)

colnames(mydata) <- c("Total.num.of.minutes.spent.by.servicemen", "Num.of.copiers.serviced") # Rename the columns
head(mydata)

plot(Total.num.of.minutes.spent.by.servicemen ~ Num.of.copiers.serviced, data=mydata)

m <- lm(Total.num.of.minutes.spent.by.servicemen ~ Num.of.copiers.serviced, data=mydata)
summary(m)


# Question 2.5(a)
confint(m, level=0.9)

# Question 2.5(d)

# Given values from regression summary
beta_estimate <- 15.0352
standard_error <- 0.4831
df <- 43  # degrees of freedom = n - 2

# Compute t-statistic
t_statistic <- (beta_estimate - 14) / standard_error

# Compute the one-sided p-value
p_value <- 1 - pt(t_statistic, df)

p_value


# Question 2.5(e)

# Question 2.14(a)
mean_service_time_CI <- predict(m, newdata = data.frame(Num.of.copiers.serviced = 6), interval = "confidence", level = 0.90)
mean_service_time_CI

# Question 2.14(b)
predict_interval <- predict(m, newdata = data.frame(Num.of.copiers.serviced = 6), interval = "prediction", level = 0.90)
predict_interval



