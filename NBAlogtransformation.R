getwd()  # Ask for current working directory
setwd("/Users/jcroman/Dropbox/SDSU_courses/STAT510") # Set working directory ; Use your own path
getwd()  # Verify working directory

nba <- read.csv(file="NBA2015Data.csv", header=TRUE)

names(nba) # column names

# We won't use attach(nba) in this example


plot(nba$MIN, nba$PTS)

hist(nba$MIN)
hist(nba$PTS)  

# Let's model log(PTS) vs MIN; log = natural log 

# Be careful a few players have PTS=0
which(nba$PTS==0)

# Let's look at their data

nba[which(nba$PTS==0), ]

# These players played 4 games of less; let's remove them from the data set

nba.r <- nba[-which(nba$PTS==0), ]

dim(nba)
dim(nba.r)  # 6 players have been removed

plot(nba.r$MIN, log(nba.r$PTS))

log.fit <- lm(log(PTS) ~ MIN, data=nba.r)
summary(log.fit)
abline(log.fit, col="red")

# Fit in original scale

plot(nba.r$MIN, nba.r$PTS)

original.fit <- function(x){	
y.hat <- coef(log.fit)[1] + coef(log.fit)[2] * x 
return(exp(y.hat))
}

x <- seq(from=min(nba.r$MIN), to=max(nba.r$MIN), by=0.01)  # Create sequence of points

lines(x, original.fit(x), col="red")

# I'll discuss in class how to intepret the regression coefficients in the original scale. 




