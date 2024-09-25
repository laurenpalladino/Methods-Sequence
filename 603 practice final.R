#Lauren Palladino practice final 2

source("http://mperess.cc.stonybrook.edu/r0.r")
library(haven)
library(stargazer)
housevote <- read_dta("Downloads/housevote.dta")
head(housevote)
attach(housevote)

#1.a OLS
lm1<-lm(incshare~growth+unem+inflat+midterm)
summary(lm1)
acf0(lm1$res, lag.max=50)
adf.test0(incshare, t)

#1.b NW
nw0<-lm0(incshare~growth+unem+inflat+midterm, se="nw", timeid=t)
print.model(nw0)
acf0(nw0$res, lag.max=50)

#1.c ARMA
ar1<-lm.arma(incshare~growth+unem+inflat+midterm, t, 0, 0)
print.model(ar1)

#1d interact
lm2<-lm(incshare~growth*repincpres+unem*repincpres+inflat*repincpres)
summary(lm2)
stargazer(lm2)

#2
house <- read_dta("Downloads/house_expanded.dta")
attach(house)

#2.a
lm3<-lm(inc_2p_share~inc_exp+ch_exp)
summary(lm3)

#2.b
fresh1<-(house$inc_fresh==1)
fresh1<-as.numeric(fresh1)
fresh2<-(house$inc_fresh==2)
fresh2<-as.numeric(fresh2)
fresh3<-(house$inc_fresh==3)
fresh3<-as.numeric(fresh3)

lm4<-lm(inc_2p_share~inc_exp+ch_exp+ch_pr_office+ch_stateleg+ch_formerhouse+fresh1+fresh2+fresh3)
summary(lm4)

#2c
inc_exp_log=log(1+inc_exp)
ch_exp_log=log(1+ch_exp)

lm5<-lm(inc_2p_share~inc_exp_log+ch_exp_log+ch_pr_office+ch_stateleg+ch_formerhouse+fresh1+fresh2+fresh3)
summary(lm5)

#2d

lmcluster <- lm0(inc_2p_share~inc_exp_log+ch_exp_log+ch_pr_office+
                   ch_stateleg+ch_formerhouse+fresh1+fresh2+fresh3,se="cluster",unitid=state_cd)
print.model(lmcluster)

#2e
inc_exp_log_lag=log(1+inc_exp_m1)
ch_exp_log_lag=log(1+ch_exp_m1)
iv<-ivreg0(inc_2p_share~inc_exp_log+ch_exp_log+ch_pr_office+ch_stateleg+ch_formerhouse+fresh1+fresh2+fresh3|
             inc_exp_log_lag+ch_exp_log_lag+ch_pr_office+ch_stateleg+ch_formerhouse+fresh1+fresh2+fresh3,
           se="cluster", unitid=state_cd, timeid=year)

print.model(iv)

#2.vii

Report and interpret the results of the 2SLS regression?

#2.viii
lmfe1<-lm.fe(inc_2p_share~inc_exp_log+ch_exp_log+ch_pr_office+
              ch_stateleg+ch_formerhouse+fresh1+fresh2+fresh3+as.factor(year)|state_cd|0)
print.model(lmfe1)

#2.ix
lmfe2<-lm.fe(inc_2p_share~inc_exp_log+ch_exp_log+ch_pr_office+
               ch_stateleg+ch_formerhouse+fresh1+fresh2+fresh3+as.factor(year)|state_cd|0|state_cd)
print.model(lmfe2)
