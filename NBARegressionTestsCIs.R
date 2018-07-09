# Read data from txt file
nba <- read.csv(file.choose(), header = T)
attach(nba)

# Fit a linear model to the data   
fit <- lm(PTS ~ MIN)
summary(fit)  # Get estimates, standard errors, and hypothesis test p-values = Pr(>|t|) for H_0: beta_i = 0 vs H_1: beta_i != 0 


# Calculate the 95% for the regression coefficients (beta_0 and beta_1)
confint(fit, level = 0.95)  


# Individual with MIN = 20
new.MIN <- data.frame(MIN = 20)
predict(fit, new.MIN, se.fit = TRUE)  # obtain PTS predicted value its standard error (se.fit)

# CI for E(PTS|MIN=20) 
CI <- predict(fit, new.MIN, interval = "confidence")
CI

# PI for new value of PTS for a player with MIN=20
PI <- predict(fit, new.MIN, interval = "prediction")
PI

detach(nba)