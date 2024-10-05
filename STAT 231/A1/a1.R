install.packages('tidyverse')

# Question 1
filepath <- "~/Desktop/STAT 231/a1/gas.csv"
gas <- readr::read_csv(
  filepath,
  col_types = readr::cols("c","n","n",readr::col_date(format = "%m/%d/%Y"))
  )
dplyr::glimpse(gas)
?readr::read_csv
# c)
gasS <- dplyr::filter(gas,car=="S")
dplyr::glimpse(gasS)
gasH <- dplyr::filter(gas,car=="H")
dplyr::glimpse(gasH)


# Question 3
# a)
mean(gasS$gas)
# b)
median(gasS$gas)
# c)
var(gasS$gas)
# d)
sd(gasS$gas)
# e)
range(gasS$gas)
# f)
IQR(gasS$gas)
# g) i)
hist(gasS$km)
# h)
boxplot(gasS$gas)
# i) i)
plot(gasS$km,gasS$gas, xlab = "km", ylab = "gas")
# i) iii)
cor(gasS$km,gasS$gas)


# Question 4
# g)
mean(gasS$gas)
# h)
# Generate some data
set.seed(424242)
x <- gasS$gas
# Create a function to compute the normal log-likelihood for these at a vector 
# of values of mu
loglik <- function(mu) {
  ll <- numeric(length(mu))
  for (i in 1:length(ll)) ll[i] <- sum(dnorm(x,mean=mu[i],sd=11,log=TRUE))
  ll
}
# Construct 1000 values will be plotted to get a nice, smooth curve.
xx <- seq(15,80,length.out=1000)
# Compute the values to be plotted
vv <- loglik(xx)-loglik(mean(gasS$gas))
# Plot it
plot(xx,vv,type='l')
# Put a vertical line at x = mean
u <- mean(x)
abline(v = u)


# Question 5
# b)
a <- gasS$gas
n <- length(a)
uhat <- mean(a)
shat <- sqrt(sum((a - uhat)^2)/n)
t <- table(cut(a, breaks = c(-Inf, 20, 30, 40, 50, 60, 70, 80, Inf), right = FALSE))
ai <- n*(pnorm(20, uhat, shat)-pnorm(-Inf, uhat, shat))
bi <- n*(pnorm(30, uhat, shat)-pnorm(20, uhat, shat))
ci <- n*(pnorm(40, uhat, shat)-pnorm(30, uhat, shat))
di <- n*(pnorm(50, uhat, shat)-pnorm(40, uhat, shat))
ei <- n*(pnorm(60, uhat, shat)-pnorm(50, uhat, shat))
fi <- n*(pnorm(70, uhat, shat)-pnorm(60, uhat, shat))
gi <- n*(pnorm(80, uhat, shat)-pnorm(70, uhat, shat))
hi <- n*(pnorm(Inf, uhat, shat)-pnorm(80, uhat, shat))
expected <- c(ai, bi, ci, di, ei, fi, gi, hi)
d <- data.frame(t,expected)
d$Var1 <- as.character(d$Var1)
d[nrow(d) + 1,] = c("Total",n,sum(expected))
knitr::kable(d, col.names = c("Interval","Observed", "Expected"))
# c)
par(mfrow=c(1,2))
uhat2 <- mean(a)
shat2 <- sqrt(sum((a - uhat)^2)/n)
# Arbitrary x values, a fine grid, so the line looks nice:
xx <- seq(10,90,length.out = 1e03)
# Compute the appropriate normal pdf at those values:
yy <- dnorm(xx,uhat2,shat2)
# Produce a histogram of the observed frequencies
hist(a,main = "Histogram of gas", xlab = "gas", freq = FALSE, breaks = c(20,30,40,50,60,70,80))
lines(xx,yy)
# Produce barplot
barplot(table(cut(a, breaks = c(20, 30, 40, 50, 60, 70, 80), right = TRUE))/n)
# d)
par(mfrow=c(1,2))
# Produce a plot of the ECDF of gas
plot(ecdf(gasS$gas),main="ecdf(gas)")
uhat3 <- mean(a)
shat3 <- sqrt(sum((a - uhat)^2)/n)
# Arbitrary x values, a fine grid, so the line looks nice:
xxx <- seq(10,90,length.out = 1e03)
# Compute the appropriate normal pdf at those values:
yyy <- pnorm(xxx,uhat3,shat3)
lines(xxx,yyy)
# Produce a plot of the CDF
plot(c(20,30,40,50,60,70,80),c(0,2/20,4/20,9/20,19/20,20/20,1),pch=20,xlab="cutpoints",ylab="manualcdf")
lines(xxx,yyy)
# e)
par(mfrow=c(1,2))
# Produce a plot using the qqnorm and qqline function
qqnorm(gasS$gas)
qqline(gasS$gas)
# Produce a plot by manually computing the values that go on the x and y axis
l <- gasS$gas
qqx <- numeric(length(l))
for (i in 1:length(qqx)) qqx[i] <- qnorm(i/(length(l)+1))
qqy <- numeric(length(gasS$gas))
for (i in 1:length(qqy)) qqy[i] <- quantile(gasS$gas,(i/((length(l)+1))))
plot(qqx, qqy,ylim=c(20,60),xlim=c(-2,2))
a <- mean(l)
b <- sqrt(sum((l - a)^2)/n)
abline(a,b)

