# Simulate fake SBPs (systolic blood pressure)
sample.size <- 1000
x <-rlnorm(sample.size, meanlog=log(110), sdlog=0.13)

# Set model parameters (and pretend they're unknown)
beta.zero <- 2.25
beta.one <- 0.64
sigma <- 5


# Random errors 
epsilon <- rnorm(sample.size, mean=0, sd=sigma)
# Get the fake DBPs (as a function of SBP and LDL)
y  <-  beta.zero + beta.one * x + epsilon

plot(x, y, xlab="Fake SBP", ylab="Fake DBP", main="Simulated Blood Pressure")
# add the true regression line
true.line <- beta.zero + beta.one * x
lines(x, true.line, col="red", lty=1)

# Let's pretend we don't know the parameter values 
# To estimate the parameters we run a regression
fit <- lm(y~x)
# Compare estimates to true parameter values (the residual standard error is the estimate of the conditional standard deviation of y given x (sigma))
summary(fit)  

abline(fit, col="green", lty=2)
legend("topleft", legend=c("true", "estimated"), lty=1:2, col=c("red", "green"))
# 95% confidence intervals
confint(fit, level=0.95)
# 99% confidence intervals
confint(fit, level=0.99)


# predicted values 
predict(fit)

#  alternate code
fit$fitted

# make prediction for x=100, 110, 120
# create new data set with new x's
new.data <- data.frame(x = c(100,110,120))
# predict y for the new x's
predict(fit, new.data, se.fit = TRUE)

# obtain confidence intervals for the (conditional) expected value of y
CI<- predict(fit, new.data, interval = "confidence")
CI
# obtain prediction intervals for new y observations
PI <- predict(fit, new.data, interval = "prediction")
PI

# Let's create a plot that includes the points, the regression lines, and the confidence and prediction bands.

# get confidence intervals
conf.limits<- predict(fit, interval = "confidence")
# get prediction intervals
pred.limits<- predict(fit, interval = "prediction")


# Plot fake data
plot(x,y, xlab="Fake SBP", ylab="Fake DBP", main="Simulated Blood Pressure")
# add the true regression line
lines(x, true.line, col="red", lty=1)
# add the regression line estimate
abline(fit, col="green", lty=2)

#add confidence intervals  (single CIs - these are *not* joint CIS)
lines(x, conf.limits[, 2], col="blue", lty=2)
lines(x, conf.limits[, 3], col="blue", lty=2)

#add prediction intervals
lines(x, pred.limits[, 2], col="purple", lty=2)
lines(x, pred.limits[, 3], col="purple", lty=2)

# Make legend
legend("topleft", legend=c("true", "estimated", "confidence intervals", "prediction intervals"), lty=c(1, 2, 2, 2), col=c("red", "green", "blue", "purple"))


install.packages("investr")
library(investr)

?plotFit

# Joint confidence and prediction bands
mydata <- data.frame(x, y)
g <- length(mydata)
fit <- lm(y~x, data=mydata)
plotFit(fit, interval="both", level=0.95, adjust="Scheffe", k=g, shade=TRUE)
