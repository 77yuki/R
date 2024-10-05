install.packages('tidyverse')

filepath <- "~/Desktop/STAT 231/a1/gas.csv"
gas <- readr::read_csv(
  filepath,
  col_types = readr::cols("c","n","n",readr::col_date(format = "%m/%d/%Y"))
  )
dplyr::glimpse(gas)
gasS <- dplyr::filter(gas,car=="S")
dplyr::glimpse(gasS)


# Question 2
# b)
mean(gasS$gas)
var(gasS$gas)
sd(gasS$gas)

# f)
set.seed(54534)
n <- nrow(gasS)
B <- 1e04 # Number of simulations to do, an arbitrary "large" number
mn <- 47 # Known
ss <- 11 # Known
# Generate a sample of sample means
samps <- numeric(B)
for (b in 1:B) {
  thesample <- rnorm(n,mn,ss)
  samps[b] <- mean(thesample)
}
# fi)
hist(samps, freq=FALSE, xlim = c(35, 60))
xx <- seq(35,60,length.out = 1e03)
yy <- dnorm(xx,mn,ss/sqrt(n))
lines(xx,yy)
# fii)
set.seed(53498)
n <- nrow(gasS)
B <- 1e04 # Number of simulations to do, an arbitrary "large" number
mn <- 47 # Known
ss <- 11 # Known
samps2 <- numeric(B)
for (b in 1:B) {
  thesample <- rnorm(n,mn,ss)
  samps2[b] <- var(thesample)
}
s <- sd(gasS$gas)
hist(samps2, freq=FALSE, ylim = c(0,0.012), xlim = c(0,350))
xx2 <- seq(0,360,length.out = 1e03)
yy2 <- (xx2^((n-1)/2-1)*((n-1)/(2*s^2))^((n-1)/2)*exp(-xx2*(n-1)/(2*s^2)))/gamma((n-1)/2)
lines(xx2, yy2)
# fiii)
set.seed(75294)
n <- nrow(gasS)
B <- 1e04 # Number of simulations to do, an arbitrary "large" number
mn <- 47 # Known
ss <- 11 # Known
samps3 <- numeric(B)
for (b in 1:B) {
  thesample <- rnorm(n,mn,ss)
  samps3[b] <- (mean(thesample) - mn)/(sd(thesample)/sqrt(n))
}
hist(samps3, freq=FALSE, ylim = c(0, 0.4),xlim = c(-6, 6))
xx3 <- seq(-6,6,length.out = 1e03)
yy3 <- dt(xx3,n-1)
lines(xx3,yy3)


# Question 3
# a)
a <- qnorm((1+0.95)/2)
m <- mean(gasS$gas)
n <- nrow(gasS)
ss <- 11 # Known
m - a*ss/sqrt(n)
m + a*ss/sqrt(n)

# b)
n <- nrow(gasS)
b <- qt((1+0.95)/2, n - 1)
m <- mean(gasS$gas)
sd <- sd(gasS$gas)
m - b*sd/sqrt(n)
m + b*sd/sqrt(n)

# c)
n <- nrow(gasS)
c <- qchisq((1-0.95)/2, n - 1)
d <- qchisq((1+0.95)/2, n - 1)
sd <- sd(gasS$gas)
(n-1)*sd^2/d
(n-1)*sd^2/c

# d)
n <- nrow(gasS)
c <- qchisq((1-0.95)/2, n - 1)
d <- qchisq((1+0.95)/2, n - 1)
sd <- sd(gasS$gas)
sqrt((n-1)*sd^2/d)
sqrt((n-1)*sd^2/c)

# e)
loglik <- function(mu) {
  ll <- numeric(length(mu))
  for (i in 1:length(ll)) ll[i] <- sum(dnorm(gasS$gas,mean=mu[i],sd=11,log=TRUE))
  ll
}
logrellik <- function(mu) loglik(mu) - loglik(mean(gasS$gas))
# Construct the values that you want to plot it at. I'll plot 1000 values, to
# get a nice, smooth curve.
xx <- seq(15,80,length.out=1000)
# Compute the values to be plotted
vv <- -2*logrellik(xx)
# Plot it
plot(xx,vv,type='l')
abline(h = qnorm((1+0.95)/2))

# f)
uniroot(function(x) -2*logrellik(x)-qchisq(0.95,1),lower=40,upper=46)
uniroot(function(x) -2*logrellik(x)-qchisq(0.95,1),lower=50,upper=55)

# g)
# gi)
set.seed(435)
n <- nrow(gasS)
mn <- 47 # Known
ss <- 11 # Known
B <- 1e04
intlower <- intupper <- covr <- numeric(B)
zval <- qnorm(.975)
for (b in 1:B) {
  samp <- rnorm(n,mn,ss)
  intlower[b] <- mean(samp) - zval * ss/sqrt(n) # Treating ss as known, so it appears in the interval
  intupper[b] <- mean(samp) + zval * ss/sqrt(n)
  # Regardless of whether we pretend mu is known or not, calculate the coverage
  # using the true value of mu.
  covr[b] <- intlower[b] <= mn & mn <= intupper[b]
}
mean(covr)
# Plot the first 100 simulated intervals
numplot <- 100
plot(c(intlower[1],intupper[1]),c(1,1),
     xlim = c(min(intlower),max(intupper)),
     ylim = c(1,numplot),
     type='l',lty='dashed')
abline(v = mn)
for (i in 1:numplot) {
  if (covr[i] == 1) {
    lines(c(intlower[i],intupper[i]),c(i,i))
  } else {
    lines(c(intlower[i],intupper[i]),c(i,i),col='red')
  }
}
# gii)
set.seed(435)
n <- nrow(gasS)
mn <- 47 # Known
ss <- 11 # Known
B <- 1e04
intlower <- intupper <- covr <- numeric(B)
zval <- qnorm(.975)
for (b in 1:B) {
  samp <- rnorm(n,mn,ss)
  intlower[b] <- mean(samp) - zval * sd(samp)/sqrt(n) 
  intupper[b] <- mean(samp) + zval * sd(samp)/sqrt(n)
  # Regardless of whether we pretend mu is known or not, calculate the coverage
  # using the true value of mu.
  covr[b] <- intlower[b] <= mn & mn <= intupper[b]
}
mean(covr)
# Plot the first 100 simulated intervals
numplot <- 100
plot(c(intlower[1],intupper[1]),c(1,1),
     xlim = c(min(intlower),max(intupper)),
     ylim = c(1,numplot),
     type='l',lty='dashed')
abline(v = mn)
for (i in 1:numplot) {
  if (covr[i] == 1) {
    lines(c(intlower[i],intupper[i]),c(i,i))
  } else {
    lines(c(intlower[i],intupper[i]),c(i,i),col='red')
  }
}
# giii)
set.seed(435)
n <- nrow(gasS)
mn <- 47 # Known
ss <- 11 # Known
B <- 1e04
intlower <- intupper <- covr <- numeric(B)
tval <- qt(.975,n-1)
for (b in 1:B) {
  samp <- rnorm(n,mn,ss)
  intlower[b] <- mean(samp) - tval * sd(samp)/sqrt(n) 
  intupper[b] <- mean(samp) + tval * sd(samp)/sqrt(n)
  # Regardless of whether we pretend mu is known or not, calculate the coverage
  # using the true value of mu.
  covr[b] <- intlower[b] <= mn & mn <= intupper[b]
}
mean(covr)
# Plot the first 100 simulated intervals
numplot <- 100
plot(c(intlower[1],intupper[1]),c(1,1),
     xlim = c(min(intlower),max(intupper)),
     ylim = c(1,numplot),
     type='l',lty='dashed')
abline(v = mn)
for (i in 1:numplot) {
  if (covr[i] == 1) {
    lines(c(intlower[i],intupper[i]),c(i,i))
  } else {
    lines(c(intlower[i],intupper[i]),c(i,i),col='red')
  }
}

# h)
n <- nrow(gasS)
b <- qt((1+0.95)/2, n - 1)
m <- mean(gasS$gas)
sd <- sd(gasS$gas)
2*(b*sd/sqrt(n))

# i)
uniroot(function(nn) 2*(qt((1+0.95)/2, nn - 1)*sd/sqrt(nn))-3, lower=2, upper=10000000)

# j)
# jI)
n <- nrow(gasS)
b <- qt((1+0.95)/2, n - 1)
m <- mean(gasS$gas)
sd <- sd(gasS$gas)
m - b*sd*sqrt(1+1/n)
m + b*sd*sqrt(1+1/n)
# jII)
set.seed(4723698)
n <- nrow(gasS)
mn <- 47 # Known
ss <- 11 # Known
B <- 1e04
intlower <- intupper <- covr <- numeric(B)
tval <- qt(.975,n-1)
for (b in 1:B) {
  samp <- rnorm(n,mn,ss)
  intlower[b] <- mean(samp) - tval * sd(samp)*sqrt(1+1/n) 
  intupper[b] <- mean(samp) + tval * sd(samp)*sqrt(1+1/n) 
  # Regardless of whether we pretend mu is known or not, calculate the coverage
  # using the true value of mu.
  y <- rnorm(1, mn, ss)
  covr[b] <- intlower[b] <= y & y <= intupper[b]
}
mean(covr)