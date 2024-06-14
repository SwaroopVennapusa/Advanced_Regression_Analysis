# Step 1
install.packages("car")
library(car)

# Step 2
data(Duncan)
head(Duncan)
str(Duncan)
  
# Step 3
help(Duncan)
str(Duncan)
table(Duncan$type)
hist(Duncan$income, main="Histogram of Income", xlab="Income")
hist(Duncan$education, main="Histogram of Education", xlab="Education")
hist(Duncan$prestige, main="Histogram of Prestige", xlab="Prestige")

# Step 4
pairs(Duncan)

# Step 5
m <- lm(prestige ~ education + income + type, data=Duncan)
summary(m)

# Step 6
vif(m)

# Step 7
residualPlots(m, ~1, type="rstudent", id=list(labels=row.names(Duncan)))

# Step 8
hist(residuals(m))
qqPlot(m)

# Step 9
influencePlot(m, id=list(labels=row.names(Duncan)))
Cooks.d <- cooks.distance(m)
p <- 5
n <- nrow(Duncan)
percentile <- 100 * pf(q=Cooks.d, df1=p, df2=n-p)
data.frame(Duncan, Cooks.d=round(Cooks.d, 3), percentile=round(percentile, 1))

# Step 10
dfbetas(m)
