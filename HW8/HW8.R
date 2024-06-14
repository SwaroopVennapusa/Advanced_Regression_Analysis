# Set working directory to the location of the datasets
setwd("/home/swaroop/Downloads/Assignments/STP530/HW8")

# Read the data files
data1.20 <- read.table("CH01PR20.txt", header=FALSE) # Assuming there's a header
data8.15 <- read.table("CH08PR15.txt", header=FALSE) # Assuming there's a header

combined_data <- cbind(data1.20, data8.15) 
colnames(combined_data) <- c("X_i1", "Y", "X_i2")


model <- lm(Y ~ X_i1 + X_i2 + X_i1:X_i2, data=combined_data)
summary(model)  # This will provide a summary of the regression including coefficients.

