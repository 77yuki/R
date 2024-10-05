# Question 2
setwd("~/Desktop/STAT 371/a2")
ceocomp=read.table("ceocomp.txt",header = TRUE)
attach(ceocomp)
set.seed(20884580)
my.dat = ceocomp[sample(nrow(ceocomp),75),]

# a)
plot(my.dat)
# b)
positive <- subset(my.dat, PROF > 0)
attach(positive)
bg <- as.factor(BACKGRD)
ceocomplm <- lm(COMP ~ AGE+EDUCATN+bg+TENURE+EXPER+SALES+VAL+PCNTOWN+PROF)
summary(ceocomplm)
# e)
# iii)
ate <- lm(AGE~EDUCATN+bg+TENURE+EXPER+SALES+VAL+PCNTOWN+PROF)
summary(ate)
vif<-1/(1-0.4092)
vif
# f)
newdata <- data.frame(AGE=65,EDUCATN=1,bg="2",TENURE=22,EXPER=8,SALES=3250,
                      VAL=8.2,PCNTOWN=2,PROF=112)
predict(ceocomplm,newdata,interval='prediction',level=.95)
# g)
calculatepvalue <- 1-pf(3.658,12,46)
calculatepvalue