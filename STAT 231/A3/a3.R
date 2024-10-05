install.packages('tidyverse')

filepath <- "~/Desktop/STAT 231/a1/gas.csv"
gas <- readr::read_csv(
  filepath,
  col_types = readr::cols("c","n","n",readr::col_date(format = "%m/%d/%Y"))
  )
dplyr::glimpse(gas)



# Question 1
# a)
with(gas, plot(gas ~ km))

# b)
mod <- lm(gas ~ km, data = gas)
summary(mod)

# c)
x <- gas$km
y <- gas$gas
xb <- mean(x)
yb <- mean(y)
sxx <- sum((x - xb)^2)
sxy <- sum((x - xb)*(y - yb))
bh <- sxy/sxx 
n <- nrow(gas)
syy <- sum((y - yb)^2)
se <- sqrt((syy-bh*sxy)/(n-2))
sebeta <- se/sqrt(sxx)
bh + c(-1,1)*qt(.975,n-2) * sebeta
ah <- yb - bh*xb
sealpha <- se*sqrt(1/n+xb^2/sxx)
ah + c(-1,1)*qt(.975,n-2) * sealpha

# e)
d <- abs(ah-0)/sealpha
d
2*(1-pt(d,n-2))


# g)
xx <- seq(200,800,length.out=1e03)
datforpred <- data.frame(km = xx)
mypredictions_confidence <- predict(mod,datforpred,interval="confidence")
plot(gas$km,gas$gas,pch=20)
lines(xx,mypredictions_confidence[ ,'fit'])
confbands <- predict(mod,
                     newdata = data.frame(km=xx),
                     interval = 'confidence')
predbands <- predict(mod,
                     newdata = data.frame(km=xx),
                     interval = 'prediction')
lines(xx,confbands[ ,2],lty='dashed')
lines(xx,confbands[ ,3],lty='dashed')
lines(xx,predbands[ ,2],lty='dotdash')
lines(xx,predbands[ ,3],lty='dotdash')

# h)
# h) I)
residualvalues <- residuals(mod)
sdr <- residualvalues/se
fittedvalues <- fitted(mod)
residualvalues
sdr
fittedvalues
# h) II)
plot(gas$km,sdr,ylab = "standardized residual",xlab = "x",ylim = c(-3,3))
abline(h=0)
# h) III)
plot(fittedvalues,sdr,ylab="standardized residual",xlab="Muhat",ylim=c(-3,3))
abline(h=0)
# h) IV)
qqnorm(sdr)
qqline(sdr)
# h) V)
cutpoints <- c(19,42,45,50,55,65)
intervals <- paste0(paste0("[",cutpoints,",",sep=""),
                    c(cutpoints[2:length(cutpoints)],"+\U221E"),")",sep="")
intervals <- c(paste0("(-\U221E,",cutpoints[1],")"),intervals)
observed <- c(0,table(cut(gas$gas,cutpoints)),0)
mn <- mean(gas$gas)
n <- nrow(gas)
ss <- sqrt((n-1)/n)*sd(gas$gas)
expected <- nrow(gas) * ( pnorm(c(cutpoints,Inf),mn,ss) - pnorm(c(-Inf,cutpoints),mn,ss))
tab_notfull <- data.frame(
  Interval = c(intervals,"Total"),
  Observed = c(observed,nrow(gas)),
  Expected = c(expected[1:2],rep(0,length(intervals)-2),nrow(gas))
)
tab_full <- tab_notfull
tab_full$Observed <- c(observed,nrow(gas))
tab_full$Expected <- c(expected,nrow(gas))
knitr::kable(tab_full,digits = 2)
pvalue <- 1-pchisq(5.3804,1)
pvalue


# i)
# i) I)
x <- gas$km
y <- gas$gas
mod2 <- lm(gas~0+km,data=gas)
betahat <- sum(y*x)/sum(x^2)
betahat
# i) II)
n <- nrow(gas)
a <- qt((1+0.95)/2,n-1)
seb <- sqrt(1/(n-1)*sum((y-betahat*x)^2))
lb <- betahat-a*seb/sqrt(sum(x^2))
ub <- betahat+a*seb/sqrt(sum(x^2))
lb
ub



# Question 2
# b)
gasS <- dplyr::filter(gas,car=="S")
dplyr::glimpse(gasS)
ns <- nrow(gasS)
ss <- sd(gasS$gas)
d <- qchisq((1+0.95)/2, ns-1)
c <- qchisq((1-0.95)/2, ns-1)
lbs <- sqrt((ns-1)*ss^2/d)
ubs <- sqrt((ns-1)*ss^2/c)
lbs
ubs
gasH <- dplyr::filter(gas,car=="H")
dplyr::glimpse(gasH)
nh <- nrow(gasH)
sh <- sd(gasH$gas)
d <- qchisq((1+0.95)/2, nh-1)
c <- qchisq((1-0.95)/2, nh-1)
lbh <- sqrt((nh-1)*sh^2/d)
ubh <- sqrt((nh-1)*sh^2/c)
lbh
ubh

# c)
us <- mean(gasS$gas)
uh <- mean(gasH$gas)
a <- qt((1+0.95)/2,ns+nh-2)
sp <- sqrt(((ns-1)*ss^2+(nh-1)*sh^2)/(ns+nh-2))
lb <- us-uh-a*sp*sqrt(1/ns+1/nh)
up <- us-uh+a*sp*sqrt(1/ns+1/nh)
lb
up

# d)
ad <- qnorm((1+0.95)/2)
lbd <- us-uh-ad*sqrt(ss^2/ns+sh^2/nh)
ubd <- us-uh+ad*sqrt(ss^2/ns+sh^2/nh)
lbd
ubd

# e)
d <- abs(us-uh)/(sp*sqrt(1/ns+1/nh))
d
pvalue <- 2*(1-pt(d,df=ns+nh-2))
pvalue