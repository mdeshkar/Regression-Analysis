getwd()  # Ask for current working directory
setwd("/Users/jcroman/Dropbox/SDSU_courses/STAT510") # Set working directory ; Use your own path
getwd()  # Verify working directory

nba <- read.csv(file="NBA2015Data.csv", header=TRUE)


attach(nba)
names(nba) # column names

plot(MIN, PTS)

fit <- lm(PTS ~ MIN)
summary(fit)
abline(fit)


fit2 <- lm(PTS ~ MIN + I(MIN^2))
summary(fit2)

# abline doesn't work here

# Another way of adding the regression line to the original plot

linear.fit <- function(x){	
y.hat <- coef(fit)[1] + coef(fit)[2] * x 
return(y.hat)
}

x <- seq(from=min(MIN), to=max(MIN), by=0.01)  # Create sequence of points

lines(x, linear.fit(x), col="red")

# Add quadratic fit

quadratic.fit <- function(x){	
y.hat <- coef(fit2)[1] + coef(fit2)[2] * x + coef(fit2)[3] * x^2
return(y.hat)
}

lines(x, quadratic.fit(x), col="green")


# Try cubic fit?

fit3 <- lm(PTS ~ MIN + I(MIN^2) + I(MIN^3))
summary(fit3)


# Add cubic fit

cubic.fit <- function(x){	
y.hat <- coef(fit3)[1] + coef(fit3)[2] * x + coef(fit3)[3] * x^2 + coef(fit3)[4] * x^3
return(y.hat)
}

lines(x, cubic.fit(x), col="blue")


# Try adding a MIN^4 term?

fit4 <- lm(PTS ~ MIN + I(MIN^2) + I(MIN^3) + I(MIN^4))
summary(fit4)

fit4b <- lm(PTS ~ poly(MIN, degree=4, raw=TRUE)) # Shorter code
summary(fit4b)  # Should be the same as summary(fit4)

last.fit <- function(x){	
y.hat <- coef(fit4)[1] + coef(fit4)[2] * x + coef(fit4)[3] * x^2 + coef(fit4)[4] * x^3 + coef(fit4)[5] * x^4
return(y.hat)
}

lines(x, last.fit(x), col="purple")

# The output suggests that the cubic model is enough. 



# Final model
plot(MIN, PTS)
lines(x, cubic.fit(x), col="blue")


