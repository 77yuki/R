setwd("~/Desktop/STAT 371/a2")
ceocomp=read.table("ceocomp.txt",header = TRUE)
attach(ceocomp)
set.seed(20884580)
my.dat = ceocomp[sample(nrow(ceocomp),75),]
positive <- subset(my.dat, PROF > 0)
attach(positive)
bg <- as.factor(BACKGRD)
ceocomplm <- lm(COMP ~ AGE+EDUCATN+bg+TENURE+EXPER+SALES+VAL+PCNTOWN+PROF)
summary(ceocomplm)

# 1)
plot(fitted(ceocomplm), residuals(ceocomplm))
qqnorm(residuals(ceocomplm))

# 2)
# a)
hist(COMP)
# b)
summary(ceocomplm)
refitm <- lm(log(COMP) ~ AGE+EDUCATN+bg+TENURE+EXPER+SALES+VAL+PCNTOWN+PROF)
summary(refitm)

# 3)
# a)
plot(SALES,log(COMP))
# b)
plot(log(SALES),log(COMP))
# c)
refmagain <- lm(log(COMP) ~ AGE+EDUCATN+bg+TENURE+EXPER+log(SALES)+log(VAL)+
                  log(PCNTOWN)+log(PROF))
summary(refmagain)

# 4)
plot(fitted(refmagain), residuals(refmagain))
qqnorm(residuals(refmagain))

# 5)
plot(fitted(refmagain), rstudent(refmagain))
qqnorm(rstudent(refmagain))

# 6)
hii <- hatvalues(refmagain)
plot(hii)
pplus1 <- length(refmagain)
n <- length(my.dat)
twoh <- 2*pplus1/59
twoh

# 7)
highestindex <- which.max(hii)
highestindex
AGE
EDUCATN
bg
TENURE
EXPER
log(SALES)
log(VAL)
log(PCNTOWN)
log(PROF)

# 8)
plot(cooks.distance(refmagain),ylim=c(0,0.3))
max(cooks.distance(refmagain))

# 9)
# a)
remodel1 <- lm(log(COMP) ~ AGE+EDUCATN+TENURE+EXPER+log(SALES)+log(VAL)+
                log(PCNTOWN)+log(PROF))
summary(remodel1)
remodel2 <- lm(log(COMP) ~ AGE+EDUCATN+TENURE+log(SALES)+log(VAL)+log(PCNTOWN)
               +log(PROF))
summary(remodel2)
remodel3 <- lm(log(COMP) ~ AGE+EDUCATN+log(SALES)+log(VAL)+log(PCNTOWN)
               +log(PROF))
summary(remodel3)
remodel4 <- lm(log(COMP) ~ EDUCATN+log(SALES)+log(VAL)+log(PCNTOWN)+log(PROF))
summary(remodel4)
remodel5 <- lm(log(COMP) ~ EDUCATN+log(VAL)+log(PCNTOWN)+log(PROF))
summary(remodel5)
remodel6 <- lm(log(COMP) ~ EDUCATN+log(VAL)+log(PCNTOWN))
summary(remodel6)
# b)
library(leaps)
newpositive <- positive
newpositive$PROF <- log(PROF)
newpositive$SALES <- log(SALES)
newpositive$VAL <- log(VAL)
newpositive$PCNTOWN <- log(PCNTOWN)
newpositive <- newpositive[,-1]
leaps(newpositive[,-3],log(COMP), method=c('adjr'),nbest=2,
      names=names(newpositive[,-3]))
leaps(newpositive[,-3],log(COMP), method=c('Cp'),nbest=2,
      names=names(newpositive[,-3]))
cmodel <- lm(log(COMP)~EDUCATN+log(SALES)+log(VAL)+log(PCNTOWN)+log(PROF))
summary(cmodel)
# c)
remodel1 <- lm(log(COMP) ~ AGE+EDUCATN+TENURE+EXPER+log(SALES)+log(VAL)+
                 log(PCNTOWN)+log(PROF))
summary(remodel1)
cmodel <- lm(log(COMP)~EDUCATN+log(SALES)+log(VAL)+log(PCNTOWN)+log(PROF))
summary(cmodel)
MSres <- 0.4897^2
SSres <- 0.4771^2*(59-6)
calculatecp <- SSres/MSres + 2*6-59
calculatecp
# e)
anova(cmodel,refmagain)
# f)
plot(fitted(cmodel), rstudent(cmodel))
qqnorm(rstudent(cmodel))
# g)
newdata <- data.frame(AGE=65,EDUCATN=1,bg="2",TENURE=22,EXPER=8,SALES=3250,
                      VAL=8.2,PCNTOWN=2,PROF=112)
predict(cmodel,newdata,interval='prediction',level=.95)
lwb <- exp(5.320138)
upb <- exp(7.429966)
lwb
upb