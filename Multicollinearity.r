# The following examples will be discussed in class.

# Uncorrelated Predictor Variables Example from textbook p. 279. See table 7.6. 

Case <- 1:8 
Crew.Size <- c(rep(4, times=4), rep(6, times = 4))
Bonus.Pay <- rep(c(2, 2, 3, 3), times = 2)
Crew.Productivity <- c(42, 39, 48, 51, 49, 53, 61, 60)

data.frame(Case, Crew.Size, Bonus.Pay, Crew.Productivity)

cor(Crew.Size, Bonus.Pay) # X1 and X2 are uncorrelated (in the sample)


fit1 <- lm(Crew.Productivity ~ Crew.Size)
summary(fit1)

fit2 <- lm(Crew.Productivity ~ Bonus.Pay)
summary(fit2)

fit12 <- lm(Crew.Productivity ~ Crew.Size + Bonus.Pay)
summary(fit12)


anova(fit1)
anova(fit2)
anova(fit12)  # Notice that, in this example, SSR(X1|X2) = SSR(X1) and SSR(X2|X1) = SSR(X2). 


# Perfecly Correlated Variables Example from textbook p. 281. See table 7.8. 

rm(list=ls()) # Clear workspace 

X1 <- c(2, 8, 6, 10)
X2 <- 5 + 0.5 * X1  # Notice the perfect linear relationship 
Y <- c(23, 83, 63, 103)

cor(X1, X2) # X1 and X2 are perfectly correlated (in the sample)

fit1 <- lm(Y ~ X1)
summary(fit1)

fit2 <- lm(Y ~ X2)
summary(fit2)

fit12 <- lm(Y ~ X1 + X2)
summary(fit12)

# R gives a warning because the betahat solution is not unique! 
# There is an infinite number of solutions since X1 and X2 are perfectly correlated (X doesn't have full rank)

X <- model.matrix(fit12)
X
qr(X)$rank # rank is 2 < 3 
qr(t(X) %*% X)$rank 
det(t(X) %*% X)  # t(X) %*% X is not invertible 


# Highly Correlated Variables Example (minor modification of previous example)
rm(list=ls()) # Clear workspace again 

set.seed(123)
X1 <- c(2, 8, 6, 9)
X2 <- 5 + 0.5 * X1 + rnorm(4, sd=0.2) # Notice the almost perfect linear relationship 
Y <- c(23, 83, 63, 103)

cor(X1, X2) # X1 and X2 are perfectly correlated (in the sample)

cor(X1, Y)
cor(X2, Y)

fit1 <- lm(Y ~ X1)
summary(fit1)

fit2 <- lm(Y ~ X2)
summary(fit2)

fit12 <- lm(Y ~ X1 + X2)
summary(fit12)


X <- model.matrix(fit12)
X
qr(X)$rank # rank is 2 < 3 
qr(t(X) %*% X)$rank 
det(t(X) %*% X)  # t(X) %*% X is invertible 


# More realistic baseball example 

rm(list=ls()) # Clear workspace again 

mlb <- read.csv(file.choose(), header=TRUE)  # find  "MLB2015data.csv"
names(mlb)


Runs <- mlb$R  #  Runs in one season
OBP <- with(mlb, OBP * 1000)
OPS <- with(mlb, OPS * 1000)

data.frame(Runs, OBP, OPS)

par(mfrow=c(1, 3))
plot(OBP, Runs)
plot(OPS, Runs)
plot(OBP, OPS)

cor(OBP, OPS)
cor(OBP, Runs)
cor(OPS, Runs)

fit1 <- lm(Runs ~ OBP)
summary(fit1)

fit2 <- lm(Runs ~ OPS)
summary(fit2)

fit12 <- lm(Runs ~ OBP + OPS)
summary(fit12)

fit21 <- lm(OPS ~ OBP) # Only predictors 
summary(fit21)

par(mfrow=c(1, 3))
plot(OBP, Runs)
abline(fit1)
plot(OPS, Runs)
abline(fit2)
plot(OBP, OPS)
abline(fit21)


X <- model.matrix(fit12)
X
qr(X)$rank # rank is 2 < 3 
qr(t(X) %*% X)$rank 
det(t(X) %*% X)  
1 / det(t(X) %*% X) 

anova(fit1)
anova(fit2)
anova(fit12) 
anova(lm(Runs ~ OPS + OBP)) # Reverse order of predictors to get SSR(OBP | OPS)





