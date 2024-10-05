# Question 2
setwd("~/Desktop/STAT 371/a1")
name=read.table("A1_variates.txt",header = TRUE)
attach(name)


# a)
bv <- c(-250000, 50, -200, 12000, 200000) #creates the Beta vector
set.seed(20884580)
n <- length(size)
sigma <- 15000
ei <- rnorm(n,0,sigma)
y <- bv[1]+ bv[2]*size + bv[3]*age + bv[4]*employees + bv[5]*col + ei
y

# b)
A1sim.lm = lm(y~size+age+employees+col) 
summary(A1sim.lm)

# c)
matrixX <- as.matrix(cbind(rep(1,n),name))
betahat <- solve(t(matrixX)%*%matrixX)%*%t(matrixX)%*%y
betahat

# d)
confint(A1sim.lm, "size", 0.95)
