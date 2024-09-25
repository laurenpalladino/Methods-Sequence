#lauren palladino
#pol 603 final exam 

source("http://mperess.cc.stonybrook.edu/r0.r")
library(haven)
bkwecon <- read_dta("Downloads/bkwecon.dta")
View(bkwecon)
attach(bkwecon)

#1.i
lm1<-lm(presdemshare~gr+un+wages+presdem*gr+presdem*un+presdem*wages)
summary(lm1)

#1.v
lmfe<-lm.fe(presdemshare~gr+un+wages+presdem*gr+presdem*un+presdem*wages|county)
print.model(lmfe)

#3
vetodata <- read_dta("Downloads/veto.dta")
attach(vetodata)
View(vetodata)

#3.i
log_veto=log(1+veto)
lm2<-lm(log_veto~pres_party_h+pres_party_s+repub+cham_diff+party_diff_avg+start)
summary(lm2)

#3.iii
two_opp <- (vetodata$pres_party_h < 0.5 & vetodata$pres_party_s < 0.5)
two_opp <- as.numeric(two_opp)
one_opp <- (vetodata$pres_party_h < 0.5 & vetodata$pres_party_s > 0.5 | 
              vetodata$pres_party_s < 0.5 & vetodata$pres_party_h > 0.5)
one_opp <- as.numeric(one_opp)

lm3<-lm(log_veto~two_opp+one_opp+repub+cham_diff+party_diff_avg+start)
summary(lm3)

#3.v
dwtest(log_veto~two_opp+one_opp+repub+cham_diff+party_diff_avg+start)
acf0(lm3$res, lag.max=50)

#3.vi
ar1<-lm.arma(log_veto~two_opp+one_opp+repub+cham_diff+party_diff_avg+start, cong, 1, 0)
print.model(ar1)
acf0(ar1$res, lag.max=50)

#3.vii
veto_lag<- ts.lag(log_veto, cong)
lm4<-lm(log_veto~veto_lag+two_opp+one_opp+repub+cham_diff+party_diff_avg+start)
summary(lm4)
dwtest(log_veto~veto_lag+two_opp+one_opp+repub+cham_diff+party_diff_avg+start)
acf0(lm4$res, lag.max=50)
