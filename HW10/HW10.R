# Step 2
install.packages("car")
library(car)

# Step 3
hire.data <- read.csv("~/Downloads/Assignments/STP530/HW10/DISCRIM.csv")

# Step 4
summary(hire.data)
table(hire.data$HIRE)
table(hire.data$GENDER)
pairs(hire.data)

# Step 5
m <- glm(HIRE ~ EDUC + EXP + GENDER, data=hire.data, family=binomial)
summary(m)

# Step 8
predict(m, type="link")
predict(m, type="response")

# Step 9
predict(m, newdata=data.frame(EDUC=6, EXP=3, GENDER=1), type="response")

# Step 10
EXP.plot <- seq(0, 12, by=.1)

# GENDER == 0 & EDUC == 4 (Female, Bachelor's degree)

pi <- predict(m, newdata=data.frame(EDUC=4, EXP=EXP.plot, 
                                    GENDER=0), type="response")

plot(EXP.plot, pi, xlim=c(0, 12), ylim=c(0, 1), 
     xlab="Years of Working Experience", 
     ylab="Probability of Being Hired", 
     type='l', col='red', lty="solid", lwd=3, 
     cex.axis=1.5, cex.lab=1.5)

# GENDER == 1 & EDUC == 4 (Male, Bachelor's degree)

pi <- predict(m, newdata=data.frame(EDUC=4, EXP=EXP.plot, 
                                    GENDER=1), type="response")

lines(EXP.plot, pi, col='blue', lty="solid", lwd=3)

# GENDER == 0 & EDUC == 6 (Female, Master's degree)

pi <- predict(m, newdata=data.frame(EDUC=6, EXP=EXP.plot, 
                                    GENDER=0), type="response")

lines(EXP.plot, pi, col='red', lty="dashed", lwd=3)

# GENDER == 1 & EDUC == 6 (Male, Master's degree)
pi <- predict(m, newdata=data.frame(EDUC=6, EXP=EXP.plot, 
                                    GENDER=1), type="response")
lines(EXP.plot, pi, col='blue', lty="dashed", lwd=3)

legend(x="topleft", legend=c("Female, Bachelor's", 
                             "Male, Bachelor's", "Female, Master's", 
                             "Male, Master's"), 
       col=c("red", "blue", "red", "blue"), 
       lty=c("solid", "solid", "dashed", "dashed"))




# Part 2
# Step 6
b_k = 1.1549  # Point estimate for EDUC
s_b_k = 0.6023  # Standard error for EDUC
z_value = 1.96  # z-value for 95% confidence interval

# Calculating the confidence interval
lower_bound = b_k - z_value * s_b_k
upper_bound = b_k + z_value * s_b_k

lower_bound; upper_bound

# Step 7
my.pred <- predict(m, newdata=data.frame(EDUC=6, EXP=3, 
                                         GENDER=1), level=.95, type="link", se.fit=T)

z.crit <- qnorm(p=.975)

LL.logit <- my.pred$fit - z.crit * my.pred$se.fit
UL.logit <- my.pred$fit + z.crit * my.pred$se.fit

# Then convert the two limits to the probability scale

LL.pi <- exp(LL.logit) / (1 + exp(LL.logit))
UL.pi <- exp(UL.logit) / (1 + exp(UL.logit))

c(LL.pi, UL.pi)

# Step 8
m.R <- glm(HIRE ~ EDUC + EXP, data=hire.data, family=binomial)
summary(m.R)

p_value <- 1 - pchisq(q=11.321, df=1)
p_value

# Step 9
m.N <- glm(HIRE ~ 1, data=hire.data, family=binomial)
summary(m.N)

test_statistic =  35.165 - 14.735
test_statistic

pchisq(q = 20.43, df = 3, lower.tail=F)

# Step 10
sel <- (hire.data$HIRE == 1)
hire.data[sel,]
hire.data[!sel,]

predict(m, type="response")[sel]
mean(predict(m, type="response")[sel])

predict(m, type="response")[!sel]
mean(predict(m, type="response")[!sel])

mean(predict(m, type="response")[sel]) - mean(predict(m, type="response")[!sel])

# Step 11
car::influencePlot(m)

p <- 4
n <- nrow(hire.data)
pf(q = 0.782, df1 = p, df2 = (n-p))

dfbetas(m)
