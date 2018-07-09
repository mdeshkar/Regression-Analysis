health <- read.table(file="HWdata.txt", header=TRUE)  
attach(health)

# See the notes for details. 

summary(health)

####### One sample tests 

# Parametric test

# H_0: mu = 30
# H_1: mu != 30
# where mu is the population mean BMI 

t.test (x=BMI, mu=30, alternative = "two.sided", conf.level= 0.95) 

####### Two sample tests: Independent samples

BMI.M <- BMI[GENDER=="Male"]
BMI.F <- BMI[GENDER=="Female"]

# Are the population SDs for BMI the same for both genders?  

# H_0: sigma_M = sigma_F 
# H_1: sigma_M != sigma_F 

# The follwing test uses the ratio of variances.
var.test (BMI.M, BMI.F)

# H_0: mu_M - mu_F = 0
# H_1: mu_M - mu_F != 0

# Test mean BMI difference = 0 (Male - Female)
t.test(BMI.M, BMI.F, var.equal = TRUE, paired = FALSE)

# H_0: mu_F - mu_M = 0
# H_1: mu_F - mu_M != 0

# Test mean BMI difference = 0 (Female - Male)
t.test(BMI ~ GENDER, var.equal = TRUE, paired = FALSE)  

levels(GENDER) # Look at the order
