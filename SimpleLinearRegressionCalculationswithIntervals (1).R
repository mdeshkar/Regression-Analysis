x <- c(115, 120, 145, 150, 117, 110) # weights
y <- c(65, 66, 68, 70, 64, 62)   # heights
n <- length(y)

S.xx <- sum(x^2) - n * (mean(x))^2
S.xy <- sum(x * y) - n * mean(x) * mean(y)

beta1.hat <- S.xy / S.xx
beta0.hat <- mean(y) - mean(x) * beta1.hat

beta0.hat 
beta1.hat 


y.pred <- beta0.hat + beta1.hat * x 

residuals <- y - y.pred

s <- sqrt( sum(residuals^2) / (n-2)) 

s


# SEs
s.beta1.hat <- s / sqrt(S.xx)
s.beta0.hat <- s * sqrt(  1/n + (mean(x))^2/ S.xx  )

s.beta0.hat 
s.beta1.hat 

alpha <- 0.05

# CIs 
beta0.hat + qt(1-alpha/2, df=n-2) * s.beta0.hat * c(-1,1)
beta1.hat + qt(1-alpha/2, df=n-2) * s.beta1.hat * c(-1,1)

# Calculate s_{y hat} and s_{pred} with x=140

s.y.hat <- s * sqrt(1/n + (140-mean(x))^2 / S.xx)
s.pred <- s * sqrt(1 + 1/n + (140-mean(x))^2 / S.xx)

# CI
beta0.hat + beta1.hat * 140  + qt(1-alpha/2, df=n-2) * s.y.hat * c(-1,1)
# PI 
beta0.hat + beta1.hat * 140  + qt(1-alpha/2, df=n-2) * s.pred * c(-1,1)





# Bonferroni CIs for the betas

alpha <- 0.05

beta0.hat + qt(1-alpha/4, df=n-2) * s.beta0.hat * c(-1,1)
beta1.hat + qt(1-alpha/4, df=n-2) * s.beta1.hat * c(-1,1)

# Bonferroni CIs for conditional means E(Y|X=x)

# Calculate s_{y hat} for different values of x. 

s.y.hat <- function(t){
return( s * sqrt(1/n +  (t-mean(x))^2 / S.xx  ) )
}

# Calculate CIs

beta0.hat + beta1.hat * 130 + qt(1 - alpha/6, df=n-2) * s.y.hat(130) * c(-1,1)
beta0.hat + beta1.hat * 135 + qt(1 - alpha/6, df=n-2) * s.y.hat(135) * c(-1,1)
beta0.hat + beta1.hat * 140 + qt(1 - alpha/6, df=n-2) * s.y.hat(140) * c(-1,1)




fit <- lm(y ~ x)
summary(fit)


plot(x, y, xlab="weight", ylab="height")
abline(fit, col="red")

# WH and Scheffe intervals 

# This function takes an lm fit and values of x and calculates WH confidence intervals for the conditional mean of Y given x. 
WH.CI<- function(fit, new.data, level=0.95){
  predict.out <- 	predict(fit, new.data, se.fit=TRUE)
  mean.estimate <- predict.out$fit 
  se.estimate <-	predict.out$se.fit
  df <-	predict.out$df
  W <- sqrt(2 * qf(level, df1=2, df2=df)) 
  lowerlim <- mean.estimate - W* se.estimate
  upperlim <- mean.estimate + W* se.estimate
  output <- data.frame(estimate = mean.estimate, se = se.estimate, lower=lowerlim, upper=upperlim, x=new.data)
  return(output)	
}

new.weights <- data.frame(x=seq(from=100, to=200, by=10))
WH.CI(fit, new.data = new.weights)

# This function takes an lm fit and values of x and calculates Scheffe PI intervals for new Y values. 
Scheffe.PI<- function(fit, new.data, level=0.95){
  predict.out <- 	predict(fit, new.data, se.fit=TRUE)
  mean.estimate <- predict.out$fit 
  se.estimate <-	predict.out$se.fit
  s <- predict.out$residual.scale
  se.pred <- sqrt(s^2 + se.estimate^2)
  df <-	predict.out$df
  g <- dim(new.data)[1]
  S <- sqrt(g * qf(level, df1=g, df2=df)) 
  lowerlim <- mean.estimate - S * se.pred
  upperlim <- mean.estimate + S * se.pred
  output <- data.frame(prediction = mean.estimate,  se.pred= se.pred, lower=lowerlim, upper=upperlim, x=new.data)
  return(output)	
}

Scheffe.PI(fit, new.data = new.weights)



#  Here's an R package for calculating WH-bands and Scheffe CIs

install.packages("investr")
library(investr)

?plotFit

mydata<-data.frame(x, y)
g <- length(mydata)
fit <- lm(y~x, data=mydata)

plotFit(fit, interval="confidence", level=0.95, adjust="Bonferroni", k=g, shade=TRUE) # WH confidence band 
plotFit(fit, interval="confidence", level=0.95, adjust="Scheffe", k=g, shade=TRUE) # WH confidence band 
plotFit(fit, interval="prediction", level=0.95, adjust="Scheffe", k=g, shade=TRUE) # Scheffe prediction band
plotFit(fit, interval="both", level=0.95, adjust="Scheffe", k=g, shade=TRUE) # WH and Scheffe bands

