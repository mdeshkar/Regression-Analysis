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

# Mean-centering MIN

mean(MIN)
hist(MIN)
 
c.MIN <- MIN - mean(MIN)  # mean-centered variable

fit.c <- lm(PTS ~ c.MIN)
summary(fit.c)   # How do we interpret the beta0 in this model?

# Standardizing MIN 

mean(MIN)
sd(MIN)

z.MIN <- (MIN - mean(MIN)) / sd(MIN)  # Standardized variable

fit.z <- lm(PTS ~ z.MIN)
summary(fit.z)   # How do we interpret the beta coefficients?


# This will be discussed in class.
