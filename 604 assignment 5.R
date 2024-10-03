#Lauren Palladino
#POL 604 assignment 5

library(dplyr)
library(ggplot2)

source("http://www.michaelperess.com/r0.r")
cps2004 <- read.dta("Downloads/cps2004.dta")
attach(cps2004)

#1
educ4<-as.numeric(educ4)
income<-as.numeric(income)
sex<-as.numeric(sex)
state<-as.numeric(GESTCEN)

#1a
f1<-vote_mnl04~early04+sdr_early04+edr_early04+edr_early_sdr04+edr04+close30_04+
  voterid04+educ4+black2+hispanic+voteself04+naturalized+nat_tenplus+married+oneyear+
  income+sex+age+age18to24+age75plus+polldiff04+south+ND+OR+WA
table(vote_mnl04)

mlogit1<-clogit0(f1, format="wide", exclude=1, se="cluster", unitid=state)
print.model(mlogit1)

#1c
mlogit1$beta

logit.predict<-function(beta,x)
{
  temp<-exp(t(beta)%*%x)
  temp/sum(temp)
}
x<-model.X(f1)
find_mode<-function(x){
  u<-unique(x)
  tab<-tabulate(match(x,u))
  u[tab==max(tab)]
}

x_mode<-apply(x,2,find_mode)
x_mode

logit.predict(mlogit1$beta,x_mode)

mlogit_sub<-function(beta,x1,x2){
  logit.predict(beta,x2)-logit.predict(beta,x1)
}

x_early<-x_mode
x_early[2]<-1
x_sdr_early<-x_mode
x_sdr_early[3]<-1
x_edr_early<-x_mode
x_edr_early[4]<-1
x_edr_early_sdr<-x_mode
x_edr_early_sdr[5]<-1
x_edr<-x_mode
x_edr[6]<-1
mlogit_sub(mlogit1$beta,x_mode,x_early)
mlogit_sub(mlogit1$beta,x_mode,x_sdr_early)
mlogit_sub(mlogit1$beta,x_mode,x_edr_early)
mlogit_sub(mlogit1$beta,x_mode,x_edr_early_sdr)
mlogit_sub(mlogit1$beta,x_mode,x_edr)

#1d
coef.diff<-function(beta,y_lev,i,j1,j2)
{
  k<-length(beta)/(y_lev-1)
  betamat<-cbind(t(matrix(beta,nrow=(y_lev-1),byrow=TRUE)),rep(0,k))
  return(betamat[i,j1]-betamat[i,j2])
}

coef.diff(as.vector(mlogit1$beta[,-2]),y_lev=4,i=10,j1=2,j2=3)
wald.test(coef.diff,as.vector(mlogit1$beta[,-2]),mlogit1$V,y_lev=4,i=10,j1=2,j2=3)

#2
library(haven)
ballotinit <- read_dta("Downloads/ballotinit.dta")
attach(ballotinit)

#2a
poisson1<-poisson0(ballot~citizen + econ + statinit + consinit + sigs + unlimit + 
                     circdays + singlesubject + distribution + primaryoffyear + 
                     repgov + repleg + ideology + legprof + time + minordiv + deficit)
print.model(poisson1)

#2c
negbin1<-negbin0(ballot~citizen + econ + statinit + consinit + sigs + unlimit + circdays + singlesubject +
                     distribution + primaryoffyear + repgov + repleg + ideology + legprof + time + minordiv + deficit)
print.model(negbin1)

#2e
library(pscl)
zeroinfl1<-zeroinfl(ballot~citizen + econ + statinit + consinit + sigs + unlimit + 
                      circdays + singlesubject + distribution + primaryoffyear + 
                      repgov + repleg + ideology + legprof + time + minordiv + deficit | 
                      citizen + econ + statinit + consinit + sigs + unlimit + 
                      circdays + singlesubject + distribution + primaryoffyear + 
                      repgov + repleg + ideology + legprof + time + minordiv + deficit)
summary(zeroinfl1)

#2g
zip.expect <- function(theta,xcurr,zcurr)
{
  beta <- theta[str_detect(names(coef(zeroinfl1)), "^count_")]
  gamma <- theta[str_detect(names(coef(zeroinfl1)), "^zero_")]
  exp(t(beta) %*% xcurr) / (1 + exp(t(gamma) %*% zcurr))
}

Xs <- model.X(ballot ~ citizen+ econ+ statinit+ consinit+ sigs+ unlimit+ circdays+ singlesubject+distribution+ primaryoffyear+ repgov+ repleg+ ideology+ legprof+ time+ minordiv+ deficit)
X_mean <- colMeans(Xs)
for (i in 0:5) {
  temp <- X_mean
  temp[2] <- i
  cat("When the number of citizen interest groups is",i,", the expected number of of ballot initiatives is",zip.expect(coef(zeroinfl1),temp,temp),"\n")
}

zip.prob <- function(theta,xcurr,zcurr,i)
{
  if(as.integer(i) != i | i < 0) stop("error in zip.prob -- i must be an integer")
  beta <- theta[str_detect(names(coef(zeroinfl1)), "^count_")]
  gamma <- theta[str_detect(names(coef(zeroinfl1)), "^zero_")]
  pi <- exp(t(gamma) %*% zcurr) / (1 + exp(t(gamma) %*% zcurr))
  lambda <- exp(t(beta) %*% xcurr)
  if(i == 0) {
    pi + (1 - pi) * exp(-lambda)
  } else {
    (1 - pi) * lambda^i * exp(-lambda) / factorial(i)
  }
}

count_prob <- data.frame(citizen=NULL,count=NULL,prob=NULL)
for (i in 0:5) {
  temp <- X_mean
  temp[2] <- i  
  output <- NA
  for(j in 0:12){
    output[(j+1)] <- zip.prob(coef(zeroinfl1),temp,temp,j)
  }
  count_prob <- rbind(count_prob,data.frame(citizen=i,count=0:12,prob=output))
}

ggplot(count_prob,aes(x=count,y=prob,colour=as.factor(citizen)))+
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks = 0:12)