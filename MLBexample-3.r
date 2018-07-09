mlb <- read.csv(file="MLB2015data.csv", header=TRUE)
attach(mlb)

table(Pos) # Number of players by position

plot(OBP, R, xlab="On base percentage", ylab="Runs scored")
# plot(AVG, R)

R.game <- R / G
OBP <- OBP * 1000


plot(OBP, R.game, xlab="OBP x 1000", ylab="Runs scored per game")
fit <- lm(R.game ~ OBP) 
abline(fit, col="red")
summary(fit)  

# Define group variable and separate 2B, SS and CF from the rest.

fast <- ifelse((Pos=="2B") | (Pos=="SS") | (Pos=="CF"), "Yes", "No")

# Obtain separate regression lines for each group

fit.M <- lm(R.game ~ OBP, subset=fast=="Yes")
fit.NM <- lm(R.game ~ OBP, subset=fast=="No")

summary(fit.M)
summary(fit.NM)

# Add points to an empty plot.  
plot(OBP, R.game, type="n", main="Linear relationship by Position")
points(OBP[fast =="Yes"], R.game[fast =="Yes"] , col="red")
points(OBP[fast =="No"], R.game[fast =="No"] , col="blue")

# Add regression lines with color
abline(fit.M, col="red")
abline(fit.NM, col="blue")
# Add legend
legend("topleft", c("2B or SS or CF", "other"), col=c("red", "blue"), pch = 1, title = "Position")


fit2 <- lm(R.game ~ OBP + fast)
summary(fit2)

fit.interaction <- lm(R.game ~ OBP + fast + OBP:fast)
summary(fit.interaction)


anova(fit2, fit.interaction)  # Drop interaction term?
anova(fit, fit2)  # Drop fast term?

anova(fit, fit.interaction)  # Compare simple linear model with the interaction model