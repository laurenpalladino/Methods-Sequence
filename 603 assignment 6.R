#Lauren Palladino POL 603 assignment 6

source("http://mperess.cc.stonybrook.edu/r0.r")
library(haven)
legprod <- read_dta("Downloads/legprod.dta")
attach(legprod)

#1.i
lm1 <- lm(Gridlock3 ~ Divided + Moderation + BicamDiff + PartyMandate + Budget + PublicMood)
summary(lm1)
#consistent but not efficient, F test is not valid, 

#1.ii
#check autocorrelation
acf0(lm1$res, lag.max=60)
#don't see significant autocorrelation, our data is probably iid
dwtest(Gridlock3 ~ Divided + Moderation + BicamDiff + PartyMandate + Budget + PublicMood)
#bc it's between 2-4, we need to compare the DW with 4-L and 4-U using a table
#K=7, N=24 gives us L=0.751 and U=2.208
4-2.208
4-0.751
#our DW falls between these numbers we get inconclusive results, 
#we cannot reject the null that there is no autocorrelation

#1.iii
#totally not necessary, but for the sake of learning:
ARMA0 <- lm.arma(Gridlock3 ~ Divided + Moderation + BicamDiff + PartyMandate + Budget + 
                   PublicMood, Obs, 1, 1)
print.model(ARMA0)
acf0(ARMA0$res, lag.max=50)

#1.iv
Gridlock3_lag <- ts.lag(Gridlock3, Obs)
Gridlock3
Gridlock3_lag

lm2 <- lm(Gridlock3 ~ Gridlock3_lag + Divided + Moderation + BicamDiff + PartyMandate + Budget + PublicMood)
summary(lm2)
acf0(lm2$res, lag.max=60)

#1.v
NW0 <- lm0(Gridlock3 ~ Divided + Moderation + BicamDiff + PartyMandate + Budget + 
             PublicMood, se="nw", timeid=Obs)
print.model(NW0)

