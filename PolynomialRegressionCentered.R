getwd()  # Ask for current working directory
setwd("/Users/jcroman/Dropbox/SDSU_courses/STAT510") # Set working directory ; Use your own path
getwd()  # Verify working directory

nba <- read.csv(file="NBA2015Data.csv", header=TRUE)


attach(nba)
names(nba) # column names

c.MIN <- MIN - mean(MIN)

# Compare these two models

fit<- lm(PTS ~ MIN)
summary(fit)

fit.c <- lm(PTS ~ c.MIN)
summary(fit.c)


# Compare these two models

fit2 <- lm(PTS ~ poly(MIN, degree=2, raw=TRUE))
summary(fit2)

fit2.c <- lm(PTS ~ poly(c.MIN, degree=2, raw=TRUE))
summary(fit2.c)  # Look at the beta1 estimate - it's the same as in fit1.c 

# Compare these two models

fit3<- lm(PTS ~ poly(MIN, degree=3, raw=TRUE))
summary(fit3)

fit3.c <- lm(PTS ~ poly(c.MIN, degree=3, raw=TRUE))
summary(fit3.c)





