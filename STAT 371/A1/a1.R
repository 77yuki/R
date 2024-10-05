# Question 1
setwd("~/Desktop/STAT 371/a1")
capmA1=read.csv("capmA1.csv",header = TRUE)
attach(capmA1)

n <- length(BMO)
shortBMO <- BMO[1:n-1]
rt <- diff(BMO)/shortBMO
allrft <- (GVTB10y/100 + 1)^(1/12) - 1
rft <- allrft[-1]
shortTSX <- TSX[1:n-1]
rmt <- diff(TSX)/shortTSX

# a)
yi <- rt-rft
xi <- rmt-rft
plot(xi,yi)

# b)
r <- cor(xi,yi)
r
sxx <- sum(xi^2) - sum(xi)^2 / 59
syy <- sum(yi^2) - sum(yi)^2 / 59
sxy <- sum(xi*yi) - (sum(xi)*sum(yi)) / 59
rdefinition <- sxy/(sqrt(sxx*syy))

# c)
model <- lm(yi ~ xi)
summary(model)

# e)
# i)
confint(model, "xi", 0.95)

# ii)
betahat <- 1.1192934
df <- 59 - 2
t <- qt(1-0.05/2, df)
se <- 0.1310296
ub <- betahat + t*se
lb <- betahat - t*se
ub
lb

# g)
# i)
muihat <- fitted(model) 
sh <- sqrt(sum((residuals(model))^2)/(59-2))
sh
# ii)
xbar <- sum(xi)/59
ybar <- sum(yi)/59
ah <- ybar - bh * xbar
ah
# iii)
bh <- sxy/sxx
bh
# iv)
se <- sh/sqrt(sxx)
se
# v)
pvalue <- pt(t,df)
pvalue



