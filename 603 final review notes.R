#603 practice final

source("http://mperess.cc.stonybrook.edu/r0.r")
library(haven)
newspapers <- read_dta("Downloads/newspapers.dta")
attach(newspapers)

#1
ols <- lm0(frac_pos~ gr+ un + inf)
acf0(ols$res, lag.max=50)
dwtest(frac_pos~gr + un + inf)

#parametric
#good bc more specific
ar1<-lm.arma(frac_pos~ gr+ un + inf, monthid, 1, 0)
print.model(ar1)
acf0(ar1$res, lag.max=50)

#semiparametric
#good bc don't need to worry abt specification but need large n
nw0<- lm0(frac_pos~ gr+ un + inf, se="nw", timeid=monthid)
print.model(nw0)
acf0(nw0$res, lag.max=50)

#ldv
#make ldv
frac_pos_m1 <- ts.lag(frac_pos, monthid)
ldv<- lm0(frac_pos~ frac_pos_m1 + gr+ un + inf)
print.model(ldv)
#makes sense bc prev prev months make sense to influence these relationships

#ix: ldv not reasonable bc theoretically doesn't make a lot of sense for pm to influence news coverage of economy

adf.test0(frac_pos, monthid)
pp.test0(frac_pos, monthid)
kpss.test0(frac_pos, monthid)

#make fixed effect


