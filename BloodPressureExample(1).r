# Find the data set HWdata.txt
mydata<-read.table(file.choose(), header=TRUE)
attach(mydata)

# Calculate regression lines by gender
fit.F<-lm(DBP~SBP, subset=GENDER=="Female")
fit.M<-lm(DBP~SBP, subset=GENDER=="Male")

# Add points to empty plot. Females points in red and males points in blue. 
plot(SBP, DBP, type="n", main="Linear relationship by gender")
points(SBP[GENDER=="Female"], DBP[GENDER=="Female"] , col="red")
points(SBP[GENDER=="Male"], DBP[GENDER=="Male"] , col="blue")

# Add regression lines with color
abline(fit.F, col="red")
abline(fit.M, col="blue")
# Add legend
legend("topleft", c("Female", "Male"), col=c("red", "blue"), pch = 1, title = "Gender")

fit <- lm(DBP ~ SBP)
summary(fit)

fit2 <- lm(DBP ~ SBP + GENDER)
summary(fit2)

fit.interaction <- lm(DBP ~ SBP + GENDER + SBP:GENDER)
summary(fit.interaction)


anova(fit2, fit.interaction)  # Drop interaction term?
anova(fit, fit2)  # Drop GENDER term?

anova(fit, fit.interaction)  # Compare simple linear model with the interaction model