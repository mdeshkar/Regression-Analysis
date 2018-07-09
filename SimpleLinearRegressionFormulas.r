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

s <- sqrt( sum(residuals^2) / (n - 2)) 

s


# SEs
s.beta1.hat <- s / sqrt(S.xx)
s.beta0.hat <- s * sqrt(  1 / n + (mean(x))^2 / S.xx  )

s.beta0.hat 
s.beta1.hat 

alpha <- 0.05

# CIs 
beta0.hat + qt(1 - alpha / 2, df = n - 2) * s.beta0.hat * c(-1, 1)
beta1.hat + qt(1 - alpha / 2, df = n - 2) * s.beta1.hat * c(-1, 1)

# Calculate s_{y hat} and s_{pred} with x=140

s.y.hat <- s * sqrt(1 / n + (140 - mean(x))^2 / S.xx)
s.pred <- s * sqrt(1 + 1/n + (140 - mean(x))^2 / S.xx)

# CI
beta0.hat + beta1.hat * 140  + qt(1 - alpha / 2, df = n - 2) * s.y.hat * c(-1, 1)
# PI 
beta0.hat + beta1.hat * 140  + qt(1 - alpha / 2, df = n - 2) * s.pred * c(-1, 1)





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

beta0.hat + beta1.hat * 130 + qt(1 - alpha / 6, df=n-2) * s.y.hat(130) * c(-1,1)
beta0.hat + beta1.hat * 135 + qt(1 - alpha / 6, df=n-2) * s.y.hat(135) * c(-1,1)
beta0.hat + beta1.hat * 140 + qt(1 - alpha / 6, df=n-2) * s.y.hat(140) * c(-1,1)








