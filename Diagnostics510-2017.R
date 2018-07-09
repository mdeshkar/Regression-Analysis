nba <- read.csv(file.choose(), header=TRUE)  # Use "NBA2015Data.csv"


fit <- lm(PTS ~ MIN, data=nba)

##### Basic Diagnostic Plots

par(mfrow=c(1, 2))  # plot settings

plot(x=fit$fitted.values, y=nba$PTS, main="Observed vs Predicted Values", xlab="Predicted Value", ylab="Observed Value" )

# Diagonal line
lines(x=fit$fitted.values, y=fit$fitted.values, lty=1, col="red")

plot(x=fit$fitted.values, y=fit$residuals, main="Residuals vs Predicted Values", xlab="Predicted Value", ylab="Residual" )

# Zero horizontal line

n <- length(fit$fitted)
zeros <- rep(0, length=n)
lines(x=fit$fitted.values, y=zeros, lty=1, col="red")

par(mfrow=c(1,1))  # return to default settings

###### Evaluate homoscedasticity - Non-constant error variance test

###  Breusch - Pagan Test ;  See Sections 3.6 and 6.8

library(car)

ncvTest(fit)  # Since the p-value < 0.05, we reject the hypothesis of equal (error) variances 


# Here's the long version of the test

resid.fit <- lm(fit$residuals^2 ~ nba$MIN)

SSR.resid <- anova(resid.fit)$Sum[1]
SSE <- anova(fit)$Sum[2]
n <- length(fit$residuals)

BP.stat <- (SSR.resid / 2) / (SSE/n)^2
BP.stat

# p-value 

1 - pchisq(BP.stat, df=1)


### Brown-Forsythe test ; See Sections 3.6 and 6.8


# Define groups 

high.MIN <- ifelse(nba$MIN>20, "Yes", "No")
 
resid.high <- fit$residuals[high.MIN=="Yes"]
resid.low <- fit$residuals[high.MIN=="No"]

d.high <- abs(resid.high - median(resid.high))
d.low <- abs(resid.low - median(resid.low))

t.test(x=d.high, y=d.low)

t.test(x=d.high, y=d.low, var.equal=TRUE) # Uses textbook formula

# Since the p-value < 0.05, we believe that the error variances/SDs are not equal. 


## Residual types ; See Sections 10.1 and 10.2

# 1) (Internally) studentized residuals 

# Recall the hat matrix H and the estimate of sigma

X <- model.matrix(fit)
H <- X %*% solve(t(X) %*% X) %*% t(X)
s <- summary(fit)$sigma

# The vector of studentized residuals is  

student.resid <- fit$residuals / (s * sqrt(1- diag(H)))

# 2) Studentized deleted residuals also known as externally studentized residuals 


# This is the efficient method  # See page 396

SSE <- s^2 * fit$df
multiplier <- sqrt(nrow(X) - ncol(X) - 1) / sqrt(SSE * (1- diag(H)) - fit$residuals^2)
student.del.resid <-  fit$residuals * multiplier     # notice the componentwise multiplication

# Now the easy way (using the function studres in the package MASS)

library(MASS)

# studres(fit)  

# Compare the studentized deleted residuals 
cbind(studres(fit), student.del.resid)

# Under the model assumptions, the studentized deleted residuals follow a t-distribution with n - p - 1 degrees of freedom


# qq plot for studentized residuals
qqPlot(fit, main="QQ Plot")   # can uses to check for non-normality and outliers

hist(studres(fit), freq=FALSE, main="Distribution of Studentized Residuals")  # Look at the long right tail 

# Look at the largest  (externally) studentized residuals 

tail(sort(studres(fit)))

# Assessing Outliers
outlierTest(fit) # Bonferonni p-value for most extreme obs


# Who are these players  (Obs. 412, 200, 276)?

nba[c(412, 200, 276), ]  # Not surprised?

# What are the highest PTS/game averages? 

tail(sort(nba$PTS))  

# The players with the higest PTS/gamme average were classified as outliers

# We should keep these players in the data set but consider a different model. 

# Influence Measures: Sections 10.3 - 10.6
library(stats)
influence.measures(fit)
rstandard(fit)  # Internally Studentized Residuals
rstudent(fit)   # Externally Studentized Residuals
dffits(fit)
cooks.distance(fit)
dfbetas(fit)
hatvalues(fit)
covratio(fit)
hat(model.matrix(fit), intercept = TRUE)


#####  Test for correlated Errors ; See Section 12.2 and 12.3 

# Plot residuals vs time order

plot(1:n, fit$residuals, xlab="TIme Order", ylab="Residual", main="Residuals vs Time Order", type="l")

durbinWatsonTest(fit)  
# Since p-value > 0.05, the independent errors assumption is plausible 

# Durbin-Watson Statistic

numerator <- sum((fit$residuals[2:n] - fit$residuals[1:(n-1)])^2)
denominator <-  sum(fit$residuals^2)

DW.stat <- numerator / denominator

DW.stat

# Lag-1 Autocorrelation 

first <- fit$residuals[1: (n-1)]
last <- fit$residuals[2:n]

ac.numerator <- sum((first - mean(first)) * (last - mean(last))) 
ac.denominator <-  sum((fit$residuals - mean(fit$residuals))^2)

ac.numerator / ac.denominator # Lag-1 AC

# Lag-2 Autocorrelation 

first <- fit$residuals[1: (n-2)]
last <- fit$residuals[3:n]

ac.numerator <- sum((first - mean(first)) * (last - mean(last))) 
ac.denominator <-  sum((fit$residuals - mean(fit$residuals))^2)

ac.numerator / ac.denominator # Lag-2 AC

# Auto-correlation plot

auto.cor <- acf(fit$residuals)

# Vector of auto-correlations

auto.cor$acf




