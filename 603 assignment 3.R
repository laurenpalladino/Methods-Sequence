#Lauren Palladino POL 603 assignment 3
library(car)
library(haven)
library(dplyr)
library(tidyr)
library(skedastic)
library(estimatr)
source("http://mperess.cc.stonybrook.edu/r0.r")

senate <- read_dta("Downloads/senate_expanded-1.dta")
attach(senate)

#i:
OLS <- lm(inc_2p_share ~ inc_tenure+ch_qual+inc_pos+st_uemp, data=senate)
summary(OLS)

#iv, H0: the coefficients on all four IVs =0
Fvalue<-(0.2429/4)/((1-0.2429)/290)
Fvalue

#df = k-1 (5-1=4), n-k(295-5=290)
pf(Fvalue, 4, 290, lower.tail = FALSE)
#reject the null that all coefficients are = 0

#v: verify using restricted and unrestricted R^2 (equation 237)
car::linearHypothesis(OLS, c("inc_pos = 0", "st_uemp = 0"))

OLS_res <-lm(inc_2p_share~inc_tenure+ch_qual,data=senate)
summary(OLS_res)
Fvalue2<-((0.2429-0.2405)/2)/((1-0.2429)/290)
Fvalue2
pf(Fvalue2,2,290,lower.tail = FALSE)

#vi: Test the null hypothesis that the coefficient on inc_pos is 0.5 in the population
car::linearHypothesis(OLS, c("inc_pos = 0.5"))

#vii: AIC & BIC
nrow(senate%>%drop_na(inc_2p_share, inc_tenure, inc_pos))
OLS_st_uemp_omitted <- lm(inc_2p_share~inc_tenure+ch_qual+inc_pos, data=senate%>%drop_na(st_uemp))

AIC(OLS)
BIC(OLS)
AIC(OLS_st_uemp_omitted)
BIC(OLS_st_uemp_omitted)

#viii
coef(OLS)["(Intercept)"]+6*coef(OLS)["inc_tenure"]+0*coef(OLS)["ch_qual"]+
  0.5*coef(OLS)["inc_pos"]+5*coef(OLS)["st_uemp"]
predict(OLS, newdata = data.frame(inc_tenure=6,ch_qual=0,inc_pos=0.5,st_uemp=5), interval = "confidence")

#ix
res <- resid(OLS)
ressq <- res*res
res_dataframe <- cbind(OLS$model,data.frame(ressq=residuals(OLS)^2))

# Check for heteroskedasticity
p1<-ggplot(res_dataframe,aes(x=inc_tenure,y=ressq))+geom_point()+geom_smooth(method=lm, se=FALSE)
p1 + xlab("Incumbent Tenure") + ylab("Squared Residuals")
p2<-ggplot(res_dataframe,aes(x=ch_qual,y=ressq))+geom_point()+geom_smooth(method=lm, se=FALSE)
p2 + xlab("Challenger Qualifications") + ylab("Squared Residuals")
p3<-ggplot(res_dataframe,aes(x=inc_pos,y=ressq))+geom_point()+geom_smooth(method=lm, se=FALSE)
p3 + xlab("Incumbent Position") + ylab("Squared Residuals")
p4<-ggplot(res_dataframe,aes(x=st_uemp,y=ressq))+geom_point()+geom_smooth(method=lm, se=FALSE)
p4 + xlab("State Unemployment Rate") + ylab("Squared Residuals")

#x: white test for heteroskedasticity
white_lm(OLS, interactions = TRUE, statonly = FALSE)

#xi: regress weighted with sq resids
wt <- 1 / lm(abs(OLS$residuals) ~ OLS$fitted.values)$fitted.values^2
wls_model <- lm(inc_2p_share ~ inc_tenure+ch_qual+inc_pos+st_uemp, data = senate, wt)
summary(wls_model)

#xii: MLE
lmhet1 <-lm.het(inc_2p_share ~ inc_tenure+ch_qual+inc_pos+st_uemp | 
                  inc_tenure+ch_qual+inc_pos+st_uemp, type=1)
lmhet2 <-lm.het(inc_2p_share ~ inc_tenure+ch_qual+inc_pos+st_uemp | 
                  inc_tenure+ch_qual+inc_pos+st_uemp, type=2)

print.models(list(OLS,"Het.-Exp. Spec."=lmhet1,"Het.-Sqr. Spec."=lmhet2)
             ,stats=c("N","r.squared","LRpval"))

#xiii: kernel density plot
plot.density(OLS$res,addnorm=T)

#xiv: check for normality
jb.test(OLS$res)
ks.test(OLS$res,"pnorm",mean=mean0(OLS$res),sd=sd0(OLS$res))

#xvii OLS with robust se
OLSrobust<- lm_robust(inc_2p_share ~ inc_tenure+ch_qual+inc_pos+st_uemp, data=senate)
OLSrobust
