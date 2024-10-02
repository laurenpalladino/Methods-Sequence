#Lauren Palladino
#POL 604 assignment 4

library(haven)
library(ggplot2)
library(Rmisc)

source("http://www.michaelperess.com/r0.r")
iraqwar <- read_dta("Downloads/iraqwar.dta")
attach(iraqwar)

#Q1.b
probit1<-probit0(iraqamistake~totantiwarthroughjune16 + gop3 + dem3 + age +
                   education4 + male + white)
print.model(probit1)

#Q1.c
probit2<-probit0(iraqamistake~totantiwarthroughjune16 + gop3 + dem3 + age + 
                   education4 + male + white + knowparty)
print.model(probit2)


#justify your answer w coefs and substantive effects:
probit.me.diff<-function(beta, k,x1,x2)
{
  me1<-dnorm(t(x1)%*%beta)*beta[k]
  me2<-dnorm(t(x2)%*%beta)*beta[k]
  me2-me1
}  

cp<-function(beta, k,x1,x2)
{
  x1n<-x1
  x1n[k]<-x1[k]+1
  cp1<-pnorm(t(x1n)%*%beta)-pnorm(t(x1)%*%beta)
  x2n<-x2
  x2n[k]<-x2[k]+1
  cp2<-pnorm(t(x2n)%*%beta)-pnorm(t(x2)%*%beta)
  cp2-cp1
}  

x2<-model.X(iraqamistake~totantiwarthroughjune16 + gop3 + dem3 + age + 
              education4 + male + white + knowparty)
x2mean<-colMeans(x2)
x2knowspartyy<-x2mean
x2knowspartyy[9]<-1
x2knowspartyn<-x2mean
x2knowspartyn[9]<-0
delta.method(probit.me.diff,probit2$coef,probit2$V,k=2,x1=x2knowspartyn,x2=x2knowspartyy)

#gives us the estimate and standard errors (the difference between the MEs is super small)

pnorm(0.000480415/0.0003901307,lower.tail=FALSE)*2
#not signifiant difference btw the two

#does CPs give us different results? no
delta.method(cp,probit2$coef,probit2$V,k=2,x1=x2knowspartyn,x2=x2knowspartyy)
pnorm(0.0005939031/0.0004999312,lower.tail=FALSE)*2
#still not stat sig

#Q1.d
probit3<-probit0(iraqamistake~totantiwarthroughjune16 + gop3 + dem3 + age + 
                   education4 + male + white + knowparty + totantiwarthroughjune16:knowparty)
print.model(probit3)

#Q2
mexico <- read_dta("Downloads/mexico.dta")
attach(mexico)

#Q2.a
f1<-app_pri~gift_pri+female+employed+urban+educ
oprobit1<-oprobit0(app_pri~gift_pri+female+employed+urban+educ)
print.model(oprobit1)

#Q2.c
oprobit.predict <- function(beta,cut,x)
{
  k <- length(cut)+1
  cut2 <- c(-9999999,cut[1:(k-1)],99999)
  mu <- (t(beta) %*% x)[1,1]
  pnorm(cut2[2:(k+1)]-mu)-pnorm(cut2[1:k]-mu)
}

#scenario 1: original
x_original <- model.X(f1)[,-1]
x_original[1,]
prob_original<-apply(x_original,1,oprobit.predict,beta=oprobit1$coef[1:5],
                     cut=oprobit1$coef[6:9])
rowMeans(prob_original)

#scenario 2: no gift
x_nogift <- x_original
x_nogift[,1]<-0
prob_nogift<-apply(x_nogift,1,oprobit.predict,beta=oprobit1$coef[1:5],
                     cut=oprobit1$coef[6:9])
rowMeans(prob_nogift)

#scenario 3: all gift
allgift <- x_original
x_allgift[,1]<-1
prob_allgift<-apply(x_allgift,1,oprobit.predict,beta=oprobit1$coef[1:5],
                     cut=oprobit1$coef[6:9])
rowMeans(prob_allgift)

#std errors:
oprobit.diff<-function(beta,x1,x2)
{
  k<-ncol(x_original)
 prob1<- rowMeans(apply(x1,1,oprobit.predict,beta=beta[1:k],cut=beta[(k+1):length(beta)]))
 prob2<- rowMeans(apply(x2,1,oprobit.predict,beta=beta[1:k],cut=beta[(k+1):length(beta)]))
 prob2-prob1
}

#difference btw original and all gift
delta.method(oprobit.diff,oprobit1$coef,oprobit1$V,x1=x_original,x2=x_allgift)
#difference btw original and no gift
delta.method(oprobit.diff,oprobit1$coef,oprobit1$V,x1=x_original,x2=x_nogift)
#difference all gift and no gift
delta.method(oprobit.diff,oprobit1$coef,oprobit1$V,x1=x_allgift,x2=x_nogift)

#Q2.d interpret effects of educ~support w plot
prob_by_educ<-data.frame(educ=NULL, app_pri=NULL)
for(i in 1:5){
  x_new<-x_original
  x_new[,5]<-i
  prob_by_educ<-rbind(prob_by_educ,data.frame(educ=i, app_pri=1:5,prob=rowMeans(apply(
    x_new,1,oprobit.predict,beta=oprobit1$coef[1:5],cut=oprobit1$coef[6:9]))))
}
prob_by_educ
ggplot(prob_by_educ,aes(x=educ, y=prob, colour=as.factor(app_pri)))+
  geom_point(size=3)+geom_line(size=1)+theme(text=element_text(size=20))+
  ylim(0,0.5)+
  scale_colour_discrete(name="app_pri")

#Q2.e
prob_diff_by_educ <- data.frame(educ=NULL,app_pri=NULL, diff=NULL)
for(i in 1:5){
  n <- nrow(x_original)
  X_nogift <- x_original
  X_allgift <- x_original
  X_nogift[,c(1,5)] <- matrix(c(rep(0,n),rep(i,n)),ncol=2)
  X_allgift[,c(1,5)] <- matrix(c(rep(1,n),rep(i,n)),ncol=2)
  prob_nogift <- rowMeans(apply(X_nogift,1,oprobit.predict,beta=oprobit1$coef[1:5],cut=oprobit1$coef[6:9]))
  prob_allgift <- rowMeans(apply(X_allgift,1,oprobit.predict,beta=oprobit1$coef[1:5],cut=oprobit1$coef[6:9]))
  prob_diff <- prob_allgift-prob_nogift
  prob_diff_by_educ <- rbind(prob_diff_by_educ,data.frame(educ=i,app_pri=1:5,diff=prob_diff))
}
prob_diff_by_educ
ggplot(prob_diff_by_educ,aes(x=educ,y=diff,colour=as.factor(app_pri)))+
  geom_point(size=3)+geom_line(size=1)+theme(text = element_text(size = 20))+
  scale_colour_discrete(name = "app_pri")   

#Q2.f replicate w linear model

#replicating initial probit
lm1<-lm0(app_pri~gift_pri+female+employed+urban+educ)
print.model(lm1)

#graph 1
edu1<-summarySE(mexico, measurevar="app_pri", groupvars="educ", na.rm=TRUE)
ggplot(aes(x=educ, y=app_pri), data=edu1)+
         geom_bar(stat="identity", position="dodge", color="black", fill="gray")+
         geom_errorbar(aes(ymin=app_pri-1.96*se, ymax=app_pri+1.96*se), width=0.01)+
         theme(text=element_text(size=20))

#graph 2
edu2<-summarySE(mexico, measurevar="app_pri", groupvars=c("educ", "gift_pri"), na.rm=TRUE)
ggplot(aes(x=educ, y=app_pri, fill=factor(gift_pri)), data=edu2)+
         geom_bar(stat="identity", position="dodge")+
         geom_errorbar(aes(ymin=app_pri-1.96*se, ymax=app_pri+1.96*se), width=0.01, position=position_dodge(0.9))+
         theme(text=element_text(size=20))

