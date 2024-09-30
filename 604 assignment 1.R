#Lauren Palladino POL 604 assignment 1 

source("http://www.michaelperess.com/r0.r")
library(haven)
library(estimatr)
civwar <- read_dta("Downloads/civwar.dta")
attach(civwar)

#1.i
f1 <- war ~ lpop + polity2 + gdpen + western + eeurop + lamerica + ssafrica + asia + 
   colbrit + colfra + mtnest + muslim
lm1 <- lm0(f1)
print.model(lm1)

#1.ii
lm2 <- lm_robust(war~lpop + polity2 + gdpen + western + eeurop + lamerica + ssafrica + 
                 asia + colbrit +colfra + mtnest + muslim)
summary(lm2)

#1.iii
log(5000)
#8.517

coef(lm2)["(Intercept)"]+8.517*coef(lm2)["lpop"]+5*coef(lm2)["polity2"]+
  5*coef(lm2)["gdpen"]+0*coef(lm2)["western"]+0*coef(lm2)["eeurop"]+0*coef(lm2)["lamerica"]+
  0*coef(lm2)["ssafrica"]+1*coef(lm2)["asia"]+0*coef(lm2)["colbrit"]+0*coef(lm2)["colfra"]+
  30*coef(lm2)["mtnest"]+40*coef(lm2)["muslim"]
predict(lm2, newdata = data.frame(lpop=8.517,polity2=5,gdpen=5,western=0,
                                  eeurop=0,lamerica=0,ssafrica=0, asia=1, colbrit=0, colfra=0, mtnest=30,
                                  muslim=40), interval = "confidence")

#1.iv
predict(lm2)
hist(predict(lm2), main= "", xlab="Predictions of model 1.ii", ylab="Frequency")

#1.v
logit1 <- logit0(f1)
print.model(logit1)

#1.vi
d1 <- delta.method(odds.ratio,logit1$coef,logit1$V)
d1$theta <- d1$est
d1$ivs <- logit1$ivs[2:length(logit1$ivs)]
print.models(list("Model"=logit1,"OR"=d1),sigbase=c(0,1))

#1.vii
X1 <- model.X(f1)
d2 <- delta.method(logit.me1,logit1$coef,logit1$V,X=X1)
d2$theta <- d2$est
d2$ivs <- logit1$ivs[2:length(logit1$ivs)]
print.models(list("Logit"=logit1,"ME1"=d2,"OLS"=lm1))

#1.ix
extreme <- case_when(polity2==10 & polity2==10~1, TRUE~0)

lm3<-lm(war~lpop + polity2 + gdpen + western + eeurop + lamerica + ssafrica + 
          asia + colbrit +colfra + mtnest + muslim + extreme)
summary(lm3)