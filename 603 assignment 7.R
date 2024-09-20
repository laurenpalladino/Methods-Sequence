#Lauren Palladino POL 603 assignment 7

source("http://mperess.cc.stonybrook.edu/r0.r")
library(haven)
senate <- read_dta("Downloads/senate_expanded.dta")
attach(senate)
head(senate)

inc_spend_capita <- (inc_spend / st_pop)
ch_spend_capita <- (ch_spend / st_pop)

log_inc_spend_capita <- log(1 + inc_spend_capita)
log_ch_spend_capita <- log(1 + ch_spend_capita)

ch_qual_1 <- (ch_qual==1)
ch_qual_2 <- (ch_qual==2)
ch_qual_3 <- (ch_qual==3)
ch_qual_4 <- (ch_qual==4)

#1.i
lm1<-lm(inc_2p_share~log_inc_spend_capita+log_ch_spend_capita+
          ch_qual_1+ch_qual_2+ch_qual_3+ch_qual_4+st_uemp)
summary(lm1)

#1.ii
lm1fe<-lm.fe(inc_2p_share~log_inc_spend_capita+log_ch_spend_capita+
               ch_qual_1+ch_qual_2+ch_qual_3+ch_qual_4+st_uemp+as.factor(year)|st_name|0|st_name)
print.model(lm1fe)

#1.iv
lm1fe2<-lm.fe(inc_2p_share~log_inc_spend_capita+log_ch_spend_capita+
               ch_qual_1+ch_qual_2+ch_qual_3+ch_qual_4+st_uemp+as.factor(year)|st_name|0)
print.model(lm1fe2)

#1.v
iv <- ivreg0(inc_2p_share~log_inc_spend_capita+log_ch_spend_capita+ch_qual_1+
              ch_qual_2+ch_qual_3+ch_qual_4+st_uemp|ch_wealthy+st_pop+ch_qual_1+
               ch_qual_2+ch_qual_3+ch_qual_4+st_uemp, se="cluster", unitid=st_abr, timeid=year)
print.model(iv)

#1.vi
iv2 <- ivreg0(inc_2p_share~log_inc_spend_capita+log_ch_spend_capita+ch_qual_1+
               ch_qual_2+ch_qual_3+ch_qual_4+st_uemp|ch_wealthy+st_pop+ch_qual_1+
               ch_qual_2+ch_qual_3+ch_qual_4+st_uemp, unitid=st_abr, timeid=year)
print.model(iv2)

#2
terror <- read_dta("Downloads/terror.dta")
attach(terror)

#2.i
lm2<-lm(fgtddom~geddes1+geddes2+geddes3+geddes4+geddes5+loggni+logpop+
  logarea+gini+durable+aggsf+coldwar+ucdp_type2+ucdp_type3)
summary(lm2)

#2.iii
lm2cluster<-lm0(fgtddom~geddes1+geddes2+geddes3+geddes4+geddes5+loggni+logpop+
              logarea+gini+durable+aggsf+coldwar+ucdp_type2+ucdp_type3, 
              se="cluster", unitid=ccode)
print.model(lm2cluster)

#2.v
lm2re<-lm.re(fgtddom~geddes1+geddes2+geddes3+geddes4+geddes5+loggni+logpop+
               logarea+gini+durable+aggsf+coldwar+ucdp_type2+ucdp_type3 +
              (1 | ccode))
print.model(lm2re)

#2.vii
lm2fe<-lm.fe(fgtddom~geddes1+geddes2+geddes3+geddes4+geddes5+loggni+logpop+
               logarea+gini+durable+aggsf+coldwar+ucdp_type2+ucdp_type3 |
               ccode|0|ccode)
print.model(lm2fe)

#2.ix
wald.test(function(x)
  c(x[3]-x[2], x[4]-x[2], x[5]-x[2], x[6]-x[2]), lm2re$theta, lm2re$V)

#2.x
wald.test(function(x)
  c(x[2]-x[1], x[3]-x[1], x[4]-x[1], x[5]-x[1]), lm2fe$theta, lm2fe$V)
