#Lauren Palladino POL 603 midterm

library(haven)
library(car)
library(ggplot2)
source("http://mperess.cc.stonybrook.edu/r0.r")

spain <- read_dta("Downloads/spain.dta")
attach(spain)
View(spain)

#1a
lm1<-lm(incvote~incpoll + growth + unem +inf)
summary(lm1)

#1c
car::linearHypothesis(lm1,c("growth = 0", "unem = 0", "inf = 0"))

#1d
coef(lm1)["(Intercept)"]+0.25*coef(lm1)["incpoll"]+0.01*coef(lm1)["growth"]+
  0.30*coef(lm1)["unem"]+0.02*coef(lm1)["inf"]
predict(lm1, newdata = data.frame(incpoll=0.25, growth=0.01, unem=0.30, inf=0.02), interval = "confidence")

#1e
res <- resid(lm1)
ressq <- res*res
res_dataframe <- cbind(lm1$model,data.frame(ressq=residuals(lm1)^2))
p1<-ggplot(res_dataframe,aes(x=incpoll,y=ressq))+geom_point()+geom_smooth(method=lm, se=FALSE)
p1
p2<-ggplot(res_dataframe,aes(x=growth,y=ressq))+geom_point()+geom_smooth(method=lm, se=FALSE)
p2
p3<-ggplot(res_dataframe,aes(x=unem,y=ressq))+geom_point()+geom_smooth(method=lm, se=FALSE)
p3
p4<-ggplot(res_dataframe,aes(x=inf,y=ressq))+geom_point()+geom_smooth(method=lm, se=FALSE)
p4
white_lm(lm1, interactions = TRUE, statonly = FALSE)

#1f
plot.density(lm1$res,addnorm=T)
jb.test(lm1$res)

#1h
car::linearHypothesis(lm1,c("incpoll = 1"))
