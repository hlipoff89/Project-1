boston <- read.csv("BostonHousing.csv")


boston$NoCHAS <- ifelse(boston$CHAS==1,0,1)

chasmod <- (lm(MEDV ~ CHAS*PTRATIO, data = boston))
nochasmod <- (lm(MEDV ~ NoCHAS*PTRATIO, data = boston))

#the main effect of pupil teacher ratio is only significant if the house is by the charles river
#so when the house is by the charles river then a one unit increase in pupil teacher ratio decreses the median house value by .3632

flood <- lm(MEDV ~ TAX*CRIM, data = boston)
summary(flood)

crime.low <- (boston$CRIM-mean(boston$CRIM))+sd(boston$CRIM)
crime.high <- (boston$CRIM-mean(boston$CRIM))-sd(boston$CRIM)

summary(lm(MEDV ~ TAX*crime.high, data = boston))
summary(lm(MEDV ~TAX*crime.low, data = boston))

#the interaction is significant in both equations
#the main effect of tax is only significant when crime is low
#so when the crime is low then a larger tax rate is associated with a lower median value house since the coefficient is negative -.049173




library(probemod)
results <- jn(flood, dv="MEDV", iv="TAX", mod="CRIM", alpha = 0.05)
plot(results)
