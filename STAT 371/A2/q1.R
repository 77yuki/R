# Question 1
setwd("~/Desktop/STAT 371/a1")
name=read.table("A1_variates.txt",header = TRUE)
attach(name)
set.seed(20884580)

Beta <- c(-250000, 50, -200, 12000, 200000)
X=as.matrix(cbind(rep(1,24),name))
beta = array(0,c(5,1000)) #creates an array to store sets of estimators
for (i in 1:1000){
  y<-X%*%Beta+rnorm(24,0,15000) #X and Beta as defined in 2a) of A1
  A1sim.lm<-lm(y~size+age+employees+col)
  beta[,i]<-coef(A1sim.lm) #yields the estimates for each iteration
}


# a)
# i)
beta4hat <- beta[5,]
hist(beta4hat)
# ii)
mean(beta[5,])
# iii)
sd(beta[5,])
# iv)
model.matrix(A1sim.lm)
XtXinv = solve(t(X)%*%X) #yields the (XtX)-1 matrix
varb4 <- XtXinv[5,5]*15000^2
varb4
sdb4 <- sqrt(varb4)
sdb4


# b)
beta2hat <- beta[3,]
beta3hat <- beta[4,]
cov(beta2hat, beta3hat)
15000^2*XtXinv[3,4]




