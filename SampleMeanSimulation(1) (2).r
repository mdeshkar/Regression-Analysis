mu <- 100 # population mean
sigma <- 13 # population sd
sample.size <- 50 # sample size

sim.data <- rnorm(sample.size, mu, sigma) # Simulated N(100,13^2)
head(sim.data) # Shows only  the first components of the vector

sim.data <- round(sim.data) # IQ data (truncated)
head(sim.data) 

par(mfrow=c(1,2)) # Allows for multiple plots

curve( dnorm(x, mu, sigma), from = mu - 4 * sigma, to = mu + 4*sigma, 
       xlab="IQ score", ylab="density", main="p.d.f of the population")

curve( dnorm(x, mu, sigma / sqrt(sample.size)), from = mu - 4 * sigma, to = mu + 4*sigma, 
       xlab="IQ score", ylab="density", main="p.d.f of the sample mean")

dev.off() # This resets the "plot" options (run after you're done looking at the plots)

n.rep <- 1000 # Number of repetitions (samples drawn)
vec.mean <- rep(NA, n.rep)

for ( i in 1:n.rep) {
  temp.sample <- round(rnorm(sample.size, mu, sigma))
  vec.mean[i] <- mean(temp.sample)
}

head(vec.mean)

# Density plot estimate for the distribution of the sample mean 

plot(density(vec.mean), xlab = "sample mean - IQ score", ylab =
       "density", main = "p.d.f of the sample mean")

par(mfrow=c(1,2)) 

hist(sim.data,  prob = TRUE,  xlab = "IQ",  
     main = "Histogram of the data") 

hist(vec.mean, prob = TRUE, xlab = "sample mean", 
     main = "Histogram of the sample means")

dev.off() # This resets the "plot" options (run after you're done looking at the plots)