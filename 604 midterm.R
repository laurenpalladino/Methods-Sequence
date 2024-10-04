#Lauren Palladino
#POL 604 midterm exam

library(dplyr)
library(haven)
library(ggplot2)
source("http://www.michaelperess.com/r0.r")

#Q3
cses <- read_dta("Downloads/cses_turn.dta")
View(cses)
attach(cses)
highinfo<-NA
highinfo[info>=2]<-1
highinfo[info<2]<-0

#3a
f1<- turnout~female + educ + polar + highinfo
logit1 <- logit0(f1)
print.model(logit1)

#3b
X1 <- model.X(f1)
ME <- delta.method(logit.me1,logit1$coef,logit1$V,X=X1)
ME$theta <- ME$est
ME$ivs <- logit1$ivs[2:length(logit1$ivs)]
print.model(ME)

#3d
f2<- turnout~female + educ + polar + highinfo + polar*highinfo
logit2 <- logit0(f2)
print.model(logit2)

#3e
X2 <- model.X(turnout~female + educ + polar + highinfo)
xmean <- colMeans(X2)
x2highinfo1<-xmean
x2highinfo1[5]<-1
x2highinfo0<-xmean
x2highinfo0[5]<-0

me.diff <- function(beta,k,X)
{
  me <- dnorm(t(X) %*% beta)*beta[k]
  me
}
delta.method(me.diff,logit1$coef,logit1$V,k=2,X=x2highinfo1)
delta.method(me.diff,logit1$coef,logit1$V,k=2,X=x2highinfo0)

logit.me.diff <- function(beta,k,X1,X2)
{
  me1 <- dnorm(t(X1) %*% beta)*beta[k]
  me2 <- dnorm(t(X2) %*% beta)*beta[k]
  me2-me1
}
delta.method(logit.me.diff,logit1$coef,logit1$V,k=2,X1=x2highinfo0,X2=x2highinfo1)
pnorm(abs(0.00545157)/0.0009675957,lower.tail = FALSE)*2

#3f
#this is a futile attempt to get results to support, I wrote down what I was trying to do on the pdf I sent in
x_original <- model.X(f1)[,-1]
x_original[1,]
prob_original<-apply(x_original,1,logit.predict,beta=logit1$coef[1:5])
rowMeans(prob_original)

#4
leg <- read_dta("Downloads/statelegprod.dta")
attach(leg)

#4a
poisson1<-poisson0(bills~polar + chamdiff + upper + log_pop + splitleg + chammajdem)
print.model(poisson1)

#4c
negbin1<-negbin0(bills~polar + chamdiff + upper + log_pop + splitleg + chammajdem)
print.model(negbin1)
