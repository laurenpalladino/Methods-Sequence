#Lauren Palladino 603 assignment 4

source("http://mperess.cc.stonybrook.edu/r0.r")
library(haven)
library(dplyr)
library(car)
wages <- read_dta("Downloads/wages_full_time.dta")
attach(wages)

#i
lm1 <- lm(LogWage~Male, data=wages)
summary(lm1)

#ii
lm2 <- lm(LogWage~Male + Age + AgeSq, data=wages)
summary(lm2)
(-4.414e-02)/(2*-4.845e-04)
#max at age 45.55

#iv
age1 <- case_when(Age<34~1, TRUE~0)
age2 <- case_when(Age>=35 & Age<44 ~2, TRUE~0)
age3 <- case_when(Age>=45 & Age<64 ~3, TRUE~0) 
age4 <- case_when(Age>=65~4, TRUE~0)

lm3 <- lm(LogWage~Male + age1 + age2 + age3)
summary(lm3)

#v
library(lmtest)
resettest(lm2)
resettest(lm3)

#vi
AIC(lm2)
BIC(lm2)
AIC(lm3)
BIC(lm3)

#viii
encomptest(lm2, lm3)

#ix
jtest(lm2, lm3)

#x
coxtest(lm2, lm3)

#xi (attempt)
vuong <- LogWage ~ Male + Age + AgeSq | Male + age1 + age2 + age3
vuongtest.lm(vuong, S=1000,alpha=0.05,subset=logical(0))

#xii
SchoolYearsSq <- SchoolYears^2

lm4 <- lm(LogWage~Male + Age + AgeSq + SchoolYears + SchoolYearsSq)
summary(lm4)

#xiii
school1 <- case_when(SchoolYears<=12~1, TRUE~0)
school2 <- case_when(SchoolYears>12 & SchoolYears<=16~2, TRUE~0)
school3 <- case_when(SchoolYears>16~3, TRUE~0) 

lm5 <-lm(LogWage~Male + age1 + age2 + age3 + school1 + school2)
summary(lm5)

#xv
lm6 <-lm(LogWage~Male + school1 + school2 + school1*Male + school2*Male)
summary(lm6)

#2
civwar <- read_dta("Downloads/civwar.dta")
attach(civwar)

#2.i
lm7<-lm(war~lpop + polity2 + gdpen + western + eeurop + lamerica + ssafrica + 
          asia + colbrit +colfra + mtnest + muslim, data=civwar)
summary(lm7)

#2.ii
lm8<-lm_robust(war~lpop + polity2 + gdpen + western + eeurop + lamerica + ssafrica + 
          asia + colbrit +colfra + mtnest + muslim, data=civwar)
summary(lm8)

#2.iii
log(5000)
#8.517

coef(lm8)["(Intercept)"]+8.517*coef(lm8)["lpop"]+5*coef(lm8)["polity2"]+
  5*coef(lm8)["gdpen"]+0*coef(lm8)["western"]+0*coef(lm8)["eeurop"]+0*coef(lm8)["lamerica"]+
  0*coef(lm8)["ssafrica"]+1*coef(lm8)["asia"]+0*coef(lm8)["colbrit"]+0*coef(lm8)["colfra"]+
  30*coef(lm8)["mtnest"]+40*coef(lm8)["muslim"]
predict(lm8, newdata = data.frame(lpop=8.517,polity2=5,gdpen=5,western=0,
        eeurop=0,lamerica=0,ssafrica=0, asia=1, colbrit=0, colfra=0, mtnest=30,
        muslim=40), interval = "confidence")

#2.iv
predict(lm7)
hist(predict(lm7), main= "", xlab="Predictions of model 2.ii", ylab="Frequency")

#2.v
extreme <- case_when(polity2==10 & polity2==10~1, TRUE~0)

lm9<-lm(war~lpop + polity2 + gdpen + western + eeurop + lamerica + ssafrica + 
                  asia + colbrit +colfra + mtnest + muslim + extreme)
summary(lm9)

#2.vi
lm10<-lm_robust(war~lpop + polity2 + gdpen + western + eeurop + lamerica + ssafrica + 
                  asia + colbrit +colfra + mtnest + mtnest*gdpen + muslim, data=civwar)
summary(lm10)
car::linearHypothesis(lm10, c("mtnest*gdpen = 0"))
