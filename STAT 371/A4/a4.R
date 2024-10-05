GDP_US <- read.csv("~/Desktop/STAT 371/a4/GDP_US.csv")
attach(GDP_US)

# a)
plot(GDP,type="b",xlab="time",main="Time Series Plot of the GDP")

# b)
plot(log(GDP),type="b",xlab="time",main="Time Series Plot of the log(GDP)")

# c)
GDP_US$time <- c(1:40)
Q <- as.factor(Quarter)
gdplm <- lm(log(GDP)~Q+time,data=GDP_US)
summary(gdplm)

# d)
plot(GDP_US$time,residuals(gdplm),xlab="time",main="Time Series Plot of the Residuals")
qqnorm(residuals(gdplm))
acf(residuals(gdplm))

# e)
GDP_US$timesquared <- (c(1:40))^2
gdplmsqu <- lm(log(GDP)~Q+time+timesquared,data=GDP_US)
summary(gdplmsqu)


# g)
plot(GDP_US$time,residuals(gdplmsqu),xlab="time",main="Time Series Plot of the Residuals")
qqnorm(residuals(gdplmsqu))
acf(residuals(gdplmsqu))

# h)
DW <- sum(diff(residuals(gdplmsqu))^2)/sum(residuals(gdplmsqu)^2)
DW 

# i)
install.packages("lmtest")
library(lmtest)
dwtest (gdplmsqu)
