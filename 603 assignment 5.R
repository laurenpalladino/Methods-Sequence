#Lauren Palladino POL 603 assignment 5

source("http://mperess.cc.stonybrook.edu/r0.r")
library(haven)
library(AER)
conflict <- read_dta("Downloads/conflict.dta")
attach(conflict)

#1.ii
lm1<-lm(any_prio~gdp_g + gdp_g_l + y_0 + polity2l + ethfrac + Oil + lpopl1 + 
          lmtnest + as.factor(ccode):I(year-1978), data=conflict)
summary(lm1)
coeftest(lm1, vcov=vcovHC, type="HC1")

#1.iv
samp <- !is.na(any_prio) & !is.na(gdp_g) & !is.na(gdp_g_l) &
  !is.na(GPCP_g) & !is.na(GPCP_g_l)
FS_gdp_g0 <- lm0(gdp_g~ GPCP_g + GPCP_g_l, se="classic", subset=samp)
print.model(FS_gdp_g0)
F.test(function(x) x[2:3], FS_gdp_g0$N, FS_gdp_g0$coef, FS_gdp_g0$V)

FS_gdp_gl0 <- lm0(gdp_g_l~GPCP_g + GPCP_g_l, se="classic", subset=samp)
print.model(FS_gdp_gl0)
F.test(function(x) x[2:3], FS_gdp_gl0$N, FS_gdp_gl0$coef, FS_gdp_gl0$V)

#1.v
iv <- ivreg(any_prio~gdp_g + gdp_g_l + y_0 + polity2l + ethfrac + Oil + lpopl1 +
              lmtnest + as.factor(ccode):I(year-1978)|GPCP_g + GPCP_g_l +
              y_0 + polity2l + ethfrac + Oil + lpopl1 + lmtnest + as.factor(ccode):I(year-1978), 
            data=conflict)
coeftest(iv, vcov=vcovHC, type="HC1") #robust se bc large N

#1.vi
iv1 <- ivreg(any_prio~gdp_g + gdp_g_l + y_0 + polity2l + ethfrac + Oil + lpopl1 +
              lmtnest + I(year-1978)|GPCP_g + GPCP_g_l +
              y_0 + polity2l + ethfrac + Oil + lpopl1 + lmtnest + I(year-1978), 
            data=conflict)
coeftest(iv1, vcov=vcovHC, type="HC1") #robust se bc large N