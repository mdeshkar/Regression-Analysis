# Don't forget to choose your working directory. You can also use file.choose()
# nba <- read.csv(file="NBA2015Data.csv", header=TRUE)
 nba <- read.csv(file.choose(), header=TRUE)

attach(nba)
names(nba) # column names

plot(MIN, PTS)

fit <- lm(PTS ~ MIN)
summary(fit)
abline(fit, col="red")

Y <- PTS
X <- model.matrix(fit)
n <- length(PTS)
I <- diag(n)
J <- matrix(rep(1, n*n), nrow=n)
H <- X %*% solve(t(X)%*%X) %*% t(X)

SSTO <- t(Y) %*% (I - 1/n *J) %*% Y
SSE <- t(Y) %*% (I - H) %*% Y
SSR <- SSTO - SSE
# SSR =  t(Y) %*% (H - 1/n *J) %*% Y
c(SSTO, SSR, SSE)

MSR <- SSR / (2-1) 
MSE <- SSE / (n-2)
F0 <- MSR / MSE
P.value <- 1 - pf(F0, df1=1, df2=n-2)  # p-value is very very small

c(MSR, MSE, F0, P.value)

anova(fit) # Compare values

summary(fit) # Look at the F statistic and p-value.

# When there's one predictor variable in the model (X), the t-test for beta1 is equivalent to the F-test for H0: beta1 = 0.  

# Let's add FGP to the model

fit2 <- lm(PTS ~ MIN + FGP)
Y <- PTS
X <- model.matrix(fit2)
n <- length(PTS)
I <- diag(n)
J <- matrix(rep(1, n*n), nrow=n)
H <- X %*% solve(t(X)%*%X) %*% t(X)

SSTO <- t(Y) %*% (I - 1/n *J) %*% Y
SSE <- t(Y) %*% (I - H) %*% Y
SSR <- SSTO - SSE
# SSR =  t(Y) %*% (H - 1/n *J) %*% Y
c(SSTO, SSR, SSE)

MSR <- SSR / (3-1) 
MSE <- SSE / (n-3)
F0 <- MSR / MSE
P.value <- 1 - pf(F0, df1=2, df2=n-3)  # p-value is very very small

c(MSR, MSE, F0, P.value)

anova(fit2) # Compare values

# Note that SSR = 11857.0  + 25.3  

summary(fit2) # Look at the F statistic and p-value.


####  General approach F-test for H0: beta2=0 vs H1: beta2!=0


# Recall that the estimate of sigma is s = sqrt(SSE/n-p)
# So SSE = s^2 * (n-p)

SSE.F <- summary(fit2)$sigma^2 * fit2$df
SSE.R <- summary(fit)$sigma^2 * fit$df


# General approach F-test statistic
F1 <- ((SSE.R - SSE.F) / (fit$df - fit2$df)) / (SSE.F / fit2$df)
# P-value
P.val <- 1 - pf(F1, df1=fit$df - fit2$df, df2=fit2$df)

c(F1, P.val)

anova(fit, fit2) # Look at the F statistic and p-value
# Should we drop FGP from model 2? 


####  General approach F-test for H0: beta2=0 and beta3=0 vs H1: beta2!=0 or beta3!=0


# Recall that the estimate of sigma is s = sqrt(SSE/n-p)
# So SSE = s^2 * (n-p)

fit <- lm(PTS ~ MIN)
fit3 <- lm(PTS ~ MIN + FGP + AST)

SSE.F <- summary(fit3)$sigma^2 * fit3$df
SSE.R <- summary(fit)$sigma^2 * fit$df


# General approach F-test statistic
F1 <- ((SSE.R - SSE.F) / (fit$df - fit3$df)) / (SSE.F / fit3$df)
# P-value
P.val <- 1 - pf(F1, df1=fit$df - fit3$df, df2=fit3$df)

c(F1, P.val)

anova(fit, fit3) # Look at the F statistic and p-value
# Should we drop both FGP and AST from model 3? Or should we keep at least one of these variables? 






# Sequential Sums

# How do we obtain the F-value associated with MIN in anova(fit2)? 

fit0 <- lm(PTS ~ 1)
fit1 <- lm(PTS ~ MIN)
fit2 <- lm(PTS ~ MIN + FGP)

SSE.0 <- summary(fit0)$sigma^2 * fit0$df
SSE.1 <- summary(fit1)$sigma^2 * fit1$df
SSE.2 <- summary(fit2)$sigma^2 * fit2$df


# F-test statistic
F.MIN <- ((SSE.0 - SSE.1) / (fit0$df - fit1$df)) / (SSE.2 / fit2$df)

#  R uses (SSE.2 / fit2$df) in the denominator of the F statistic
#  instead of (SSE.1 / fit1$df)

# P-value
P.val <- 1 - pf(F.MIN, df1=fit0$df - fit1$df, df2=fit2$df)

c(F.MIN, P.val)



# Consider the model 

fit3 <- lm(PTS ~ MIN + FGP + AST)
summary(fit3)
anova(fit3)

# Let's verify the F statistics in the anova(fit3) table

fit0 <- lm(PTS ~ 1)
fit1 <- lm(PTS ~ MIN)
fit2 <- lm(PTS ~ MIN + FGP)
fit3 <- lm(PTS ~ MIN + FGP + AST)

SSE.0 <- summary(fit0)$sigma^2 * fit0$df
SSE.1 <- summary(fit1)$sigma^2 * fit1$df
SSE.2 <- summary(fit2)$sigma^2 * fit2$df
SSE.3 <- summary(fit3)$sigma^2 * fit3$df


# Sequential F-test statistic for MIN 
F1 <- ((SSE.0 - SSE.1) / (fit0$df - fit1$df)) / (SSE.3 / fit3$df)

#  R uses (SSE.3 / fit3$df) in the denominator of this F statistic

# P-value
P.val1 <- 1 - pf(F1, df1=fit0$df - fit1$df, df2=fit3$df)

round(c(F1, P.val1), digits=3)

# Sequential F-test statistic for FGP 

F2 <- ((SSE.1 - SSE.2) / (fit1$df - fit2$df)) / (SSE.3 / fit3$df)

#  R uses (SSE.3 / fit3$df) in the denominator of this F statistic

# P-value
P.val2 <- 1 - pf(F2, df1=fit1$df - fit2$df, df2=fit3$df)

round(c(F2, P.val2), digits=3)


# Sequential F-test statistic for AST (this matches the general approach F-test for H0: beta3=0 vs H1: beta3!=0)

F3 <- ((SSE.2 - SSE.3) / (fit2$df - fit3$df)) / (SSE.3 / fit3$df)

#  R uses (SSE.3 / fit3$df) in the denominator of this F statistic

# P-value
P.val3 <- 1 - pf(F3, df1=fit2$df - fit3$df, df2=fit3$df)

round(c(F3, P.val3), digits=3)

# Compare with the F values in anova(fit3)

# So R is not always using the general approach F-test as defined in the notes. The R approach is similar but uses  (SSE.3 / fit3$df) in the denominator of all the F-statistics in anova(fit3).
