# This code simulates fake data from a linear regression model with 2 continuous predictors, estimates the regression coefficients, and plots the LS plane in 3D.

# Sample size
sample.size <- 1000

# Set model parameters (and pretend they're unknown)
beta.zero <- 3.6
beta.one <- 0.6
beta.two <- 0.02
sigma <- 5

# Empty DBP (diastolic blood pressure) sample
y <- rep(NA, sample.size)

# Simulate fake SBPs (systolic blood pressure)
x1 <- rlnorm(sample.size, meanlog=log(110), sdlog=0.13)
# Simulate fake LDLs (low density lipoprotein)
x2 <- rlnorm(sample.size, meanlog=log(114), sdlog=0.31)

# Simulate random errors
epsilon <- rnorm(sample.size, mean=0, sd=sigma)

# Get the fake DBPs (as a function of SBP and LDL)

y <- beta.zero + beta.one * x1 + beta.two * x2 + epsilon


# Fit regression plane
fit <- lm(y ~ x1 + x2)
summary(fit)


# Prediction 
new.data <- data.frame(x1=110, x2=115)
predict(fit, new.data, se.fit=TRUE)  
predict(fit, new.data, interval="confidence")  # 95% CI for the conditional mean of Y given x_1=110 and x2=115
predict(fit, new.data, interval="prediction")  # 95% Prediction Interval for a new observation Ynew given x_1=110 and x2=115



# The LS plane in 3D

install.packages("scatterplot3d")
library(scatterplot3d)

scatterplot3d(x1, x2, y)
myplot <- scatterplot3d(x1, x2, y, xlab="Fake SBP", ylab="Fake LDL", zlab="Fake DBP")
# Add Least-Squares plane
myplot$plane3d(fit, col="green")

# Add segments
trans.points <- myplot$xyz.convert(x1, x2, y)
trans.predicted <- myplot$xyz.convert(x1, x2, fitted(fit))
segments(trans.points$x, trans.points$y, trans.predicted$x, trans.predicted$y, col="red", lty=2)


