# Example from the review notes
# Let Y ~ Exp(lambda)$

lambda <- 5 

# probability density function 
? dexp
curve(dexp(x, rate = 1/lambda), from = 0, to = 20, xlab = "Time", ylab = "Density", main = expression(paste(Exp(lambda),  " Density Function")))

# cumulative distribution function 
? pexp
curve(pexp(x, rate = 1/lambda), from = 0, to = 20, xlab = "Time", ylab = "Probability", main = expression(paste(Exp(lambda),  " Cumulative Distribution Function")))

# Survival curve 1 - cdf = exp(- y /lambda) 
curve(1 - pexp(x, rate = 1/lambda), from = 0, to = 20, xlab = "Time", ylab = "Probability", main = expression(paste(Exp(lambda),  " Survival Function")))

# True median time = lambda * log(2) 
lambda * log(2)

# Quantile funcion 
curve(qexp(x, rate = 1/lambda), from = 0, to = 1, xlab = "Probability", ylab = "Quantile", main = expression(paste(Exp(lambda),  " Quantile function")))
qexp(0.5, rate = 1 / lambda)  # matches the median

# Single random observation
? rexp
t <- rexp(n=1, rate = 1/lambda)
t

#  Random sample of size 1000
sample.size <- 1000 
y <- rexp(n=sample.size, rate = 1/lambda)
y

# Density estimate with true density function on top

hist(y, xlab = "Time", ylab = "Density", main = expression(paste(Exp(lambda),  " density function")), breaks=50, freq=FALSE) 

x <- seq(from = 0, to = 20, by = 0.1)
lines(x, dexp(x, rate = 1 / lambda), col="red")

summary(y) # Compare sample median and mean to the true median and mean

sd(y) # sample sd 

lambda  # True sd 

? integrate

f <- function(x){
  return(dexp(x, rate = 1 / lambda))
}

# Recall the probability example in the notes about the Exp(5) distribution

integrate(f, lower=0, upper=5)  # P(0 < Y < 5) = 1 - exp(-1)
pexp(5, rate = 1 / lambda)

# Estimate based on the sample 
sum(y < 5) / sample.size

integrate(f, lower=2, upper=Inf) #  P(Y > 2) = exp(-2/5)
1 - pexp(2, rate = 1 / lambda)

# Estimate based on the sample 
sum(y > 2) / sample.size




